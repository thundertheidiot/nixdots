{
  config,
  inputs,
  lib,
  mlib,
  pkgs,
  ...
}:
let
  inherit (lib)
    attrValues
    concatMapStringsSep
    filterAttrs
    mapAttrsToList
    mkIf
    mkMerge
    optionalString
    ;
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib.types) listOf port str;

  enabled = config.meow.wireguard.enable;
  peerName = config.meow.wireguard.peer;
  tcpPorts = config.meow.wireguard.tcpPorts;
  dnsEnabled = config.meow.wireguard.dns.enable;
  vpn = import "${inputs.self.outPath}/sops/wireguard";
  node = vpn.peers.${peerName};
  hub = vpn.peers.${vpn.hub}.address;
  resolver = vpn.peers.${vpn.dns}.address;
  trusted = concatMapStringsSep ", " (peer: peer.address) (
    attrValues (filterAttrs (name: peer: name != vpn.hub && peer.address != node.address) vpn.peers)
  );
in
{
  options.meow.wireguard = {
    enable = mkEnOpt "WireGuard client connection";
    peer = mkOpt str "" { description = "Peer name from the WireGuard inventory."; };
    tcpPorts = mkOpt (listOf port) [ ] {
      description = "TCP ports on this host that the VPN hub may connect to.";
    };
    dns.enable = mkEnOpt "Local split DNS over WireGuard";
  };

  config = mkMerge [
    (mkIf enabled {
      sops.secrets = node.module.sops.secrets;
      networking.wg-quick.interfaces.wg0 = {
        address = [ "${node.address}/32" ];
        privateKeyFile = config.sops.secrets.wg_private.path;
        mtu = 1420;
        peers = [
          {
            publicKey = vpn.peers.${vpn.hub}.publicKey;
            allowedIPs = mapAttrsToList (_: peer: "${peer.address}/32") (
              filterAttrs (peer: _: peer != peerName) vpn.peers
            );
            presharedKeyFile = config.sops.secrets."wg_psk_${peerName}".path;
            endpoint = vpn.endpoint;
            persistentKeepalive = 25;
          }
        ];
      };

      networking.nftables = {
        enable = true;
        flushRuleset = false;
        tables.wg-peer-guard = {
          family = "inet";
          content = ''
            chain input {
              type filter hook input priority -10; policy accept;
              iifname "wg0" ip saddr { ${trusted} } return
              iifname "wg0" ip saddr ${hub} ct state established,related return
            ${optionalString (tcpPorts != [ ])
              ''iifname "wg0" ip saddr ${hub} ip daddr ${node.address} tcp dport { ${
                concatMapStringsSep ", " toString tcpPorts
              } } return''
            }
              iifname "wg0" counter drop
            }
            chain forward {
              type filter hook forward priority -10; policy accept;
              iifname "wg0" ip saddr { ${trusted} } ct status dnat ct original ip daddr ${node.address} return
              iifname "wg0" ct state established,related return
              iifname "wg0" counter drop
            }
          '';
        };
      };

      networking.firewall = {
        backend = "iptables";
        package = pkgs.iptables;
        trustedInterfaces = [ "wg0" ];
      };

      systemd.services.wg-quick-wg0 = {
        requires = [ "nftables.service" ];
        after = [ "nftables.service" ];
        partOf = [ "nftables.service" ];
      };
    })

    (mkIf dnsEnabled {
      services.resolved.enable = false;
      networking.networkmanager.dns = "none";
      services.dnsmasq = {
        enable = true;
        resolveLocalQueries = true;
        settings = {
          listen-address = [
            "127.0.0.1"
            "::1"
          ];
          bind-interfaces = true;
          no-resolv = true;
          conf-file = [ ];
          server = [
            "/home/${resolver}"
            "/server/${resolver}"
            "/local/${resolver}"
            "/desktop/${resolver}"
            "1.1.1.1"
          ];
          cache-size = 1000;
        };
      };

      systemd.services.wg-dns-route = {
        description = "Fail-closed route for private VPN DNS";
        wantedBy = [ "multi-user.target" ];
        before = [
          "dnsmasq.service"
          "wg-quick-wg0.service"
        ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
        };
        script = ''
          ${pkgs.iproute2}/bin/ip route replace blackhole ${resolver}/32 metric 42760
        '';
      };
      systemd.services.dnsmasq = {
        requires = [ "wg-dns-route.service" ];
        after = [ "wg-dns-route.service" ];
      };
      systemd.services.wg-quick-wg0 = {
        requires = [ "wg-dns-route.service" ];
        after = [ "wg-dns-route.service" ];
      };
    })
  ];
}
