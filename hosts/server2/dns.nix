{
  config,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkOpt;
  inherit (lib) unique;
  inherit (lib.types) listOf str;
  vpn = import ../../sops/wireguard;
in
{
  options = {
    # Why not write abstractions here too :clueless:
    server.domains = mkOpt (listOf str) [ ] { };
  };

  config = {
    networking.firewall.interfaces.enp3s0 = {
      allowedUDPPorts = [ 53 ];
      allowedTCPPorts = [ 53 ];
    };

    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = true;
      settings = {
        listen-address = [
          "::1"
          "127.0.0.1"
          config.server.addr
        ];
        bind-dynamic = true;
        no-resolv = true;
        conf-file = [ ];
        cache-size = 1000;
        cname = [ "*.vps.home,vps.home" ];
        auth-zone = [ "vps.home" ];
        auth-server = [ "server2.home" ];
        host-record = map (domain: "${domain},${config.server.addr}") (unique config.server.domains) ++ [
          "pc.desktop,192.168.101.100"
          "vps.home,${vpn.peers.${vpn.hub}.address}"
          "server2.home,${config.server.addr}"
        ];
        local = [
          "/home/"
          "/server/"
          "/local/"
          "/desktop/"
        ];
        server = [
          "1.1.1.1"
        ];
      };
    };
  };
}
