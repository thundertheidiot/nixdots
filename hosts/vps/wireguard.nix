{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mapAttrsToList
    filterAttrs
    concatMapStringsSep
    attrValues
    ;
  vpn = import "${inputs.self.outPath}/sops/wireguard";
  hub = vpn.peers.${vpn.hub};
  clients = filterAttrs (name: _: name != vpn.hub) vpn.peers;
in
{
  imports = [ hub.module ];

  networking.wg-quick.interfaces.wg0 = {
    address = [ "${hub.address}/24" ];
    privateKeyFile = config.sops.secrets.wg_private.path;
    listenPort = vpn.port;
    peers = mapAttrsToList (name: peer: {
      publicKey = peer.publicKey;
      allowedIPs = [ "${peer.address}/32" ];
      presharedKeyFile = config.sops.secrets."wg_psk_${name}".path;
    }) clients;
  };

  networking.firewall = {
    backend = "iptables";
    package = pkgs.iptables;
    allowedUDPPorts = [ vpn.port ];
  };

  networking.nftables = {
    enable = true;
    flushRuleset = false;
    tables.wg-hub-boundary = {
      family = "inet";
      content =
        let
          addresses = concatMapStringsSep ", " (peer: peer.address) (attrValues clients);
        in
        ''
          chain forward {
            type filter hook forward priority -10; policy drop;
            iifname "wg0" oifname "wg0" ip saddr { ${addresses} } ip daddr { ${addresses} } counter accept
          }
          chain output {
            type filter hook output priority -10; policy accept;
            ip saddr { ${addresses} } counter drop
            oifname "wg0" ct state established,related return
            oifname "wg0" ip saddr ${hub.address} ip daddr ${vpn.peers.home2.address} tcp dport { 4533, 8096 } return
            oifname "wg0" counter drop
          }
        '';
      # 4533 - navidrome
      # 8096 - jellyfin
    };
  };

  # The host is trusted as a router; local services cannot initiate arbitrary peer access.
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  systemd.services.wg-quick-wg0 = {
    requires = [ "nftables.service" ];
    after = [ "nftables.service" ];
    partOf = [ "nftables.service" ];
  };
}
