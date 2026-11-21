{
  config,
  ...
}:
let
  vpnAddress = config.vpnNamespaces.airvpn.namespaceAddress;
  nya = config.meow.dns.domain;
in
{
  config = {
    services.nginx.virtualHosts = {
      "homepage.${nya}" = {
        forceSSL = true;
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

    sops.secrets."homepage_env" = {
      mode = "0400";
      owner = "root";
      group = "root";
    };

    systemd.services.homepage-dashboard.environment.HOSTNAME = "127.0.0.1";

    services.homepage-dashboard = {
      enable = true;

      environmentFiles = [
        config.sops.secrets."homepage_env".path
        config.sops.templates."media-stack-homepage".path
      ];
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
            # Don't want to dox myself :)
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
                href = "https://jellyfin.${nya}";
                widget = {
                  type = "jellyfin";
                  url = "http://127.0.0.1:8096";
                  key = "{{HOMEPAGE_VAR_JELLYFIN_API}}";
                  version = 2;
                  enableBlocks = true;
                  enableNowPlaying = false;
                };
              };
            }
            {
              "Navidrome" = {
                icon = "navidrome.svg";
                href = "https://navidrome.${nya}";
              };
            }
            {
              "FreshRSS" = {
                icon = "freshrss.svg";
                href = "https://rss.${nya}";
              };
            }
            {
              "BookOrbit" = {
                icon = "mdi-book-open-page-variant";
                href = "https://books.${nya}";
              };
            }
          ];
        }
        {
          "Downloads" = [
            {
              "Radarr" = {
                icon = "radarr.svg";
                href = "https://radarr.${nya}";
                widget = {
                  type = "radarr";
                  url = "http://${vpnAddress}:7878";
                  key = "{{HOMEPAGE_VAR_RADARR_API}}";
                  enableQueue = true;
                };
              };
            }
            {
              "Sonarr" = {
                icon = "sonarr.svg";
                href = "https://sonarr.${nya}";
                widget = {
                  type = "sonarr";
                  url = "http://${vpnAddress}:8989";
                  key = "{{HOMEPAGE_VAR_SONARR_API}}";
                  enableQueue = true;
                };
              };
            }
            {
              "Bazarr" = {
                icon = "bazarr.svg";
                href = "https://bazarr.${nya}";
                widget = {
                  type = "bazarr";
                  url = "http://${vpnAddress}:6767";
                  key = "{{HOMEPAGE_VAR_BAZARR_API}}";
                  enableQueue = true;
                };
              };
            }
            {
              "MeTube" = {
                icon = "metube.svg";
                href = "https://metube.${nya}";
              };
            }
            {
              "Soulbeet" = {
                icon = "mdi-music-box-multiple";
                href = "https://soulbeet.${nya}";
              };
            }
          ];
        }
        {
          "Management" = [
            {
              "qBittorrent" = {
                icon = "qbittorrent.svg";
                href = "https://torrent.${nya}";
                widget = {
                  type = "qbittorrent";
                  url = "http://${vpnAddress}:8080";
                  username = "{{HOMEPAGE_VAR_QBIT_USERNAME}}";
                  password = "{{HOMEPAGE_VAR_QBIT_PASSWORD}}";
                };
              };
            }
            {
              "Prowlarr" = {
                icon = "prowlarr.svg";
                href = "https://prowlarr.${nya}";
                widget = {
                  type = "prowlarr";
                  url = "http://${vpnAddress}:9696";
                  key = "{{HOMEPAGE_VAR_PROWLARR_API}}";
                };
              };
            }
          ]
          ++ [
            {
              "Soulseek" = {
                icon = "slskd.svg";
                href = "https://soulseek.${nya}";
              };
            }
          ];
        }
        {
          "Services" = [
            {
              "Forgejo" = {
                icon = "forgejo.svg";
                href = "https://git.${nya}";
              };
            }
            {
              "Redlib" = {
                icon = "redlib.svg";
                href = "https://reddit.${nya}";
              };
            }
            {
              "Home Assistant" = {
                icon = "home-assistant.svg";
                href = "https://homeassistant.${nya}";
              };
            }
            {
              "Syncthing" = {
                icon = "syncthing.svg";
                href = "https://syncthing.${nya}";
                description = "lan only";
              };
            }
            {
              "Grafana" = {
                icon = "grafana.svg";
                href = "https://dash.${nya}";
              };
            }
            {
              "Vaultwarden" = {
                icon = "vaultwarden.svg";
                href = "https://vw.meowcloud.net";
              };
            }
          ];
        }
      ];
    };
  };
}
