{
  config,
  inputs,
  ...
}: let
  # vpnAddress = config.vpnNamespaces."airvpn".namespaceAddress;
  vpnAddress = "127.0.0.1";
in {
  config = {
    server.domains = [
      "homepage.local"
      "homepage.home"
    ];

    services.nginx.virtualHosts = {
      "homepage.local" = {
        serverAliases = ["homepage.home"];
        addSSL = true;
        sslCertificate = (import "${inputs.self.outPath}/certs")."local.crt";
        sslCertificateKey = config.sops.secrets.localKey.path;
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

    sops.secrets."homepage_env".mode = "0644";

    services.homepage-dashboard = {
      enable = true;

      environmentFile = config.sops.secrets."homepage_env".path;
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
                href = "http://jellyfin.home";
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
              "Immich" = {
                icon = "immich.svg";
                href = "http://immich.home";
                widget = {
                  type = "immich";
                  url = "http://127.0.0.1:2283";
                  key = "{{HOMEPAGE_VAR_IMMICH_API}}";
                  version = 2;
                };
              };
            }
          ];
        }
        {
          "Downloads" = [
            {
              "Radarr" = {
                icon = "radarr.svg";
                href = "http://radarr.home";
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
                href = "http://sonarr.home";
                widget = {
                  type = "sonarr";
                  url = "http://${vpnAddress}:8989";
                  key = "{{HOMEPAGE_VAR_SONARR_API}}";
                  enableQueue = true;
                };
              };
            }
            {
              "Lidarr" = {
                icon = "lidarr.svg";
                href = "http://lidarr.home";
                widget = {
                  type = "lidarr";
                  url = "http://${vpnAddress}:8686";
                  key = "{{HOMEPAGE_VAR_LIDARR_API}}";
                  enableQueue = true;
                };
              };
            }
            {
              "Bazarr" = {
                icon = "bazarr.svg";
                href = "http://bazarr.home";
                widget = {
                  type = "bazarr";
                  url = "http://${vpnAddress}:6767";
                  key = "{{HOMEPAGE_VAR_BAZARR_API}}";
                  enableQueue = true;
                };
              };
            }
          ];
        }
        {
          "Management" = [
            {
              "qBittorrent" = {
                icon = "qbittorrent.svg";
                href = "http://torrent.home";
                widget = {
                  type = "qbittorrent";
                  url = "http://${vpnAddress}:8080";
                  username = "admin";
                  password = "adminadmin";
                };
              };
            }
            {
              "Prowlarr" = {
                icon = "prowlarr.svg";
                href = "http://prowlarr.home";
                widget = {
                  type = "prowlarr";
                  url = "http://${vpnAddress}:9696";
                  key = "{{HOMEPAGE_VAR_PROWLARR_API}}";
                };
              };
            }
            {
              "Firefox" = {
                description = "Firefox container permanently connected to a VPN";
                icon = "firefox.svg";
                href = "http://firefox.home";
              };
            }
            {
              "Soulseek" = {
                icon = "slskd.svg";
                href = "http://soulseek.home";
              };
            }
          ];
        }
      ];
    };
  };
}
