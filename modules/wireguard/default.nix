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
  lanResolver = inputs.self.nixosConfigurations.server2.config.server.addr;
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
    dns.enable = mkEnOpt "Private DNS over the home LAN or WireGuard when away";
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
      networking.networkmanager = {
        dns = "default";
        # Publish upstream DNS without replacing the local dnsmasq resolver.
        settings.main.rc-manager = "unmanaged";
      };
      services.dnsmasq = {
        enable = true;
        resolveLocalQueries = true;
        settings = {
          listen-address = [
            "127.0.0.1"
            "::1"
          ];
          bind-interfaces = true;
          resolv-file = "/run/NetworkManager/resolv.conf";
          # Respect NetworkManager's DNS priorities, with VPN DNS first by default.
          strict-order = true;
          clear-on-reload = true;
          conf-file = [ ];
          servers-file = "/run/wg-dns/servers";
          cache-size = 1000;
        };
      };

      # servers-file is reread on SIGHUP, unlike the main dnsmasq configuration.
      systemd.services.wg-dns-select = {
        description = "Select LAN or VPN private DNS";
        before = [ "dnsmasq.service" ];
        # NetworkManager emits enough events at boot to hit the default start limit.
        unitConfig.StartLimitIntervalSec = 0;
        serviceConfig = {
          Type = "oneshot";
          RuntimeDirectory = "wg-dns";
          RuntimeDirectoryPreserve = "yes";
        };
        script = ''
          resolver=${resolver}
          if [ -n "$(${pkgs.iproute2}/bin/ip -4 route show match ${lanResolver}/32 scope link)" ]; then
            resolver=${lanResolver}
          fi
          for zone in home server local desktop; do
            printf 'server=/%s/%s\n' "$zone" "$resolver"
          done > /run/wg-dns/servers.new
          if cmp -s /run/wg-dns/servers.new /run/wg-dns/servers; then
            rm /run/wg-dns/servers.new
            exit 0
          fi
          mv /run/wg-dns/servers.new /run/wg-dns/servers
          echo "Private DNS resolver: $resolver"
          if ${pkgs.systemd}/bin/systemctl is-active --quiet dnsmasq.service; then
            ${pkgs.systemd}/bin/systemctl kill --kill-whom=main --signal=HUP dnsmasq.service
          fi
        '';
      };
      # Recover from missed dispatcher events, including during boot and resume.
      systemd.timers.wg-dns-select = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "15s";
          OnUnitInactiveSec = "30s";
        };
      };
      networking.networkmanager.dispatcherScripts = [
        {
          source = pkgs.writeShellScript "select-private-dns" ''
            case "$2" in
              up|down|vpn-up|vpn-down|dhcp4-change|connectivity-change|reapply)
                ${pkgs.systemd}/bin/systemctl --no-block start wg-dns-select.service
                ;;
            esac
          '';
        }
      ];

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
        requires = [
          "wg-dns-route.service"
          "wg-dns-select.service"
        ];
        after = [
          "wg-dns-route.service"
          "wg-dns-select.service"
        ];
      };
      systemd.services.wg-quick-wg0 = {
        requires = [ "wg-dns-route.service" ];
        after = [ "wg-dns-route.service" ];
      };
    })
  ];
}
