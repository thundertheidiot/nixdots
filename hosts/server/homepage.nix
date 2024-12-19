{
  pkgs,
  lib,
  config,
  ...
}: {
  config = {
    server.domains = [
      "homepage.box"
    ];

    services.nginx.virtualHosts = {
      "homepage.box" = {
        root = "/fake";
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:8082";
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header Host $proxy_host;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Host $http_host;
              proxy_set_header X-Forwarded-Proto $scheme;

            '';
          };
        };
      };
    };

    meow.sops.enableSecrets = [
      "server_homepage_env"
    ];

    meow.sops.secrets."server_homepage_env".mode = "0644";

    services.homepage-dashboard = {
      enable = true;

      environmentFile = config.sops.secrets."server_homepage_env".path;
      settings = {
        title = "Homepage";
      };

      widgets = [
        {
          resources = {
            cpu = true;
            memory = true;
          };
        }
        {
          openmeteo = {
            # Don't want to dox myself ;)
            latitude = "{{HOMEPAGE_VAR_LAT}}";
            longitude = "{{HOMEPAGE_VAR_LONG}}";

            timezone = "Europe/Helsinki";
            units = "metric";

            cache = 5;
          };
        }
      ];

      services = [
        {
          "Media" = [
            {
              "Jellyfin" = {
                icon = "jellyfin.svg";
                href = "http://jellyfin.box";
                widget = {
                  type = "jellyfin";
                  url = "http://127.0.0.1:8096";
                  key = "{{HOMEPAGE_VAR_JELLYFIN_API}}";
                  enableBlocks = true;
                  enableNowPlaying = false;
                };
              };
            }
            {
              "Radarr" = {
                icon = "radarr.svg";
                href = "http://radarr.box";
                widget = {
                  type = "radarr";
                  url = "http://127.0.0.1:7878";
                  key = "{{HOMEPAGE_VAR_RADARR_API}}";
                  enableQueue = true;
                };
              };
            }
            {
              "Sonarr" = {
                icon = "sonarr.svg";
                href = "http://sonarr.box";
                widget = {
                  type = "sonarr";
                  url = "http://127.0.0.1:8989";
                  key = "{{HOMEPAGE_VAR_SONARR_API}}";
                  enableQueue = true;
                };
              };
            }
            {
              "Soulseek" = {
                icon = "slskd.svg";
                href = "http://soulseek.box";
              };
            }
          ];
        }
        {
          "Management" = [
            {
              "qBittorrent" = {
                icon = "qbittorrent.svg";
                href = "http://torrent.box";
                widget = {
                  type = "qbittorrent";
                  url = "http://127.0.0.1:8080";
                  username = "admin";
                  password = "adminadmin";
                };
              };
            }
            {
              "Prowlarr" = {
                icon = "prowlarr.svg";
                href = "http://prowlarr.box";
                widget = {
                  type = "prowlarr";
                  url = "http://127.0.0.1:9696";
                  key = "{{HOMEPAGE_VAR_PROWLARR_API}}";
                };
              };
            }
            {
              "Firefox" = {
                description = "Firefox container permanently connected to a VPN";
                icon = "firefox.svg";
                href = "http://firefox.box";
              };
            }
          ];
        }
      ];
    };
  };
}
