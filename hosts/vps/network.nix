{...}: {
  config = {
    networking = {
      hostName = "vps";
      networkmanager.enable = false;

      interfaces."enp1s0" = {
        useDHCP = false;
        ipv4.addresses = [
          {
            address = "95.216.151.56";
            prefixLength = 22;
          }
        ];
      };

      defaultGateway = {
        interface = "enp1s0";
        address = "172.31.1.1";
      };

      # Cloudflare DNS over IPv4 and IPv6
      nameservers = [
        "1.1.1.1"
        "1.0.0.1"
        "2606:4700:4700::1111"
        "2606:4700:4700::1001"
      ];
    };
  };
}
