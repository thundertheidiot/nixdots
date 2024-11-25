{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: {
  config = {
    networking.firewall.allowedUDPPorts = [53];
    networking.firewall.allowedTCPPorts = [53];

    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = true;
      settings = {
        listen-address = "::1,127.0.0.1,${config.server.addr}";
        interface = "eno0";
        resolv-file = let
          conf = pkgs.writeText "resolv.conf" ''
            nameserver 1.1.1.1
          '';
        in "${conf}";
        cache-size = 1000;
        address =
          (map (d: "/${d}/${config.server.addr}") [
            "jellyfin.box"
            "firefox.box"
            "torrent.box"
            "soulseek.box"
            "radarr.box"
            "sonarr.box"
            "prowlarr.box"
          ])
          ++ [
            "/pc.desktop/192.168.101.100"
          ];
        server = [
          "1.1.1.1"
        ];
      };
    };
  };
}
