{
  config,
  ...
}:
let
  vpnAddress = config.vpnNamespaces.airvpn.namespaceAddress;
  inherit (config.meow.dns) names;
in
{
  config = {
    services.nginx.virtualHosts = {
      ${names.homepage} = {
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
                href = "https://${names.jellyfin}";
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
                href = "https://${names.navidrome}";
              };
            }
            {
              "FreshRSS" = {
                icon = "freshrss.svg";
                href = "https://${names.freshrss}";
              };
            }
            {
              "BookOrbit" = {
                icon = "mdi-book-open-page-variant";
                href = "https://${names.bookorbit}";
              };
            }
          ];
        }
        {
          "Downloads" = [
            {
              "Radarr" = {
                icon = "radarr.svg";
                href = "https://${names.radarr}";
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
                href = "https://${names.sonarr}";
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
                href = "https://${names.bazarr}";
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
                href = "https://${names.metube}";
              };
            }
            {
              "Soulbeet" = {
                icon = "mdi-music-box-multiple";
                href = "https://${names.soulbeet}";
              };
            }
          ];
        }
        {
          "Management" = [
            {
              "qBittorrent" = {
                icon = "qbittorrent.svg";
                href = "https://${names.qbittorrent}";
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
                href = "https://${names.prowlarr}";
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
                href = "https://${names.slskd}";
              };
            }
          ];
        }
        {
          "Services" = [
            {
              "Forgejo" = {
                icon = "forgejo.svg";
                href = "https://${names.forgejo}";
              };
            }
            {
              "Home Assistant" = {
                icon = "home-assistant.svg";
                href = "https://${names.homeassistant}";
              };
            }
            {
              "Syncthing" = {
                icon = "syncthing.svg";
                href = "https://${names.syncthing}";
                description = "lan only";
              };
            }
            {
              "Grafana" = {
                icon = "grafana.svg";
                href = "https://${names.grafana}";
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
