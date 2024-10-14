{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: {
  config = {
    networking.firewall.allowedUDPPorts = [
      53
    ];
    networking.firewall.allowedTCPPorts = [
      53
    ];

    # TODO: dnsmasq incredibly slow startup?
    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = true;
      settings = {
        listen-address = "::1,127.0.0.1,${config.server.addr}";
        interface = "eno0";
        # resolv-file = let
        #   conf = pkgs.writeText "resolv.conf" ''
        #     nameserver 1.1.1.1
        #   '';
        # in "${conf}";
        address = map (d: "/${d}/${config.server.addr}") [
          "jellyfin.box"
          "saa.tana"
        ];
        server = [
          "1.1.1.1"
        ];
      };
    };
  };
}
