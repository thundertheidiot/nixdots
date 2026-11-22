{ infra, ... }:
{
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
          infra.hosts.server2.lan
        ];
        bind-dynamic = true;
        no-resolv = true;
        conf-file = [ ];
        cache-size = 1000;
        server = [ "1.1.1.1" ];
      };
    };
  };
}
