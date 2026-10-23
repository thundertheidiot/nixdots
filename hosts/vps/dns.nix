{ inputs, lib, ... }:
let
  inherit (lib) mapAttrsToList unique;
  vpn = import "${inputs.self.outPath}/sops/wireguard";
in
{
  networking.firewall.interfaces.wg0 = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;
    settings = {
      listen-address = [ vpn.peers.${vpn.hub}.address ];
      bind-dynamic = true;
      no-resolv = true;
      no-hosts = true;
      conf-file = [ ];
      cache-size = 1000;
      # Never send private names to the upstream resolver.
      local = [
        "/home/"
        "/server/"
        "/local/"
        "/desktop/"
      ];
      server = [ "1.1.1.1" ];
      host-record =
        let
          domains = inputs.self.nixosConfigurations.server2.config.server.domains;
        in
        map (domain: "${domain},${vpn.peers.home2.address}") (unique domains)
        ++ mapAttrsToList (
          name: peer:
          let
            hostname =
              {
                home = "uwu";
                home2 = "server2";
              }
              .${name} or name;
          in
          "${hostname}.home,${peer.address}"
        ) vpn.peers
        ++ [ "pc.desktop,${vpn.peers.desktop.address}" ];
    };
  };
}
