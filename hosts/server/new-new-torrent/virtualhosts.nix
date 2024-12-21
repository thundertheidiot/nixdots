{config, ...}: let
  inherit (builtins) mapAttrs;
in {
  server.domains = [
    "soulseek.local"
    "torrent.local"
    "radarr.local"
    "sonarr.local"
    "lidarr.local"
    "bazarr.local"
    "prowlarr.local"
  ];

  services.nginx.virtualHosts =
    mapAttrs (_: port: {
      root = "/fake";
      locations = {
        "/" = {
          proxyPass = "http://${config.vpnNamespaces."airvpn".namespaceAddress}:${toString port}";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Host $proxy_host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Host $http_host;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      };
    }) {
      "soulseek.local" = 5030;
      "torrent.local" = 8080;
      "radarr.local" = 7878;
      "sonarr.local" = 8989;
      "lidarr.local" = 8686;
      "bazarr.local" = 6767;
      "prowlarr.local" = 9696;
    };
}
