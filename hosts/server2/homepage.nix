{
  config,
  inputs,
  ...
}:
let
  vpnAddress = config.vpnNamespaces.airvpn.namespaceAddress;
in
{
  config = {
    server.domains = [
      "homepage.local"
      "homepage.home"
    ];

    services.nginx.virtualHosts = {
      "homepage.local" = {
        serverAliases = [ "homepage.home" ];
        forceSSL = true;
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
                href = "https://jellyfin.home";
                widget = {
                  type = "jellyfin";
                  url = "http://127.0.0.1:8096";
                  key = "{{HOMEPAGE_VAR_JELLYFIN_API}}";
                  enableBlocks = true;
                  enableNowPlaying = false;
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
                href = "https://radarr.home";
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
                href = "https://sonarr.home";
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
                href = "https://bazarr.home";
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
                href = "https://torrent.home";
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
                href = "https://prowlarr.home";
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
                href = "https://soulseek.home";
              };
            }
          ];
        }
      ];
    };
  };
}
