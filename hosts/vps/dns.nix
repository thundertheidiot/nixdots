{ infra, ... }:
{
  networking.firewall.interfaces.wg0 = {
    allowedTCPPorts = [ 53 ];
    allowedUDPPorts = [ 53 ];
  };

  services.dnsmasq = {
    enable = true;
    resolveLocalQueries = false;
    settings = {
      listen-address = [ infra.hosts.vps.wg ];
      bind-dynamic = true;
      no-resolv = true;
      no-hosts = true;
      conf-file = [ ];
      cache-size = 1000;
      server = [ "1.1.1.1" ];
    };
  };
}
