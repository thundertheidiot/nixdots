{
  config,
  lib,
  mlib,
  pkgs,
  infra,
  ...
}:
let
  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib.types) str;
  inherit (lib)
    mkIf
    mkMerge
    listToAttrs
    head
    mkForce
    getExe
    getExe'
    ;

  cfg = config.meow.server.matrix;
in
{
  options = {
    meow.server.matrix.enable = mkEnOpt "Matrix";
    meow.server.matrix.domain = mkOpt str config.meow.server.mainDomain { };
  };

  imports = [
    ./whatsapp.nix
  ];

  config = mkIf cfg.enable (mkMerge [
    {
      meow.impermanence.directories = [
        {
          path = "/var/lib/private/continuwuity";
          inherit (config.services.matrix-continuwuity) user group;
          permissions = "0700";
        }
        {
          path = config.services.matrix-continuwuity.settings.global.database_backup_path;
          inherit (config.services.matrix-continuwuity) user group;
          permissions = "0700";
        }
      ];

      meow.server.certificates = [
        "matrix.${cfg.domain}" # web ui
        cfg.domain
      ];

      sops.secrets."matrix_registration_token" = {
        owner = config.services.matrix-continuwuity.user;
        group = config.services.matrix-continuwuity.group;
        mode = "0440";
      };

      users.users."${config.services.matrix-continuwuity.user}".extraGroups = [ "turnserver" ];

      services.matrix-continuwuity = {
        enable = true;
        settings = mkMerge [
          {
            global = {
              server_name = cfg.domain;
              port = [ 8008 ];
              address = [
                "127.0.0.1"
                "::1"
              ];
              database_backup_path = "/opt/continuwuity-backups";

              allow_registration = true;
              registration_token_file = config.sops.secrets."matrix_registration_token".path;

              allow_federation = true;
              trusted_servers = [ "matrix.org" ];

              well_known = {
                client = "https://${cfg.domain}";
                server = "${cfg.domain}:443";
              };

              url_preview_domain_explicit_allowlist = [
                "tenor.com"
                "giphy.com"
                "imgflip.com"
                "cdn.discordapp.com"
                "images-ext-1.discordapp.net"
                "i.makeagif.com"
                "pbs.twimg.com"
                "fixupx.com"
                "github.com"
                "wikipedia.org"
                "cdn.lesbian.ee" # kaya image server
              ];
            };
          }
          (mkIf config.meow.server.coturn {
            global = {
              turn_secret_file = config.sops.secrets."coturn_secret".path;
              turn_uris = [
                "turns:${config.services.coturn.realm}?transport=udp"
                "turns:${config.services.coturn.realm}?transport=tcp"
              ];
            };
          })
        ];
      };

      services.nginx.clientMaxBodySize = "60M";
      services.nginx.virtualHosts."${cfg.domain}" = {
        locations = listToAttrs (
          map
            (name: {
              inherit name;
              value = {
                proxyPass = "http://127.0.0.1:${toString (head config.services.matrix-continuwuity.settings.global.port)}";
                recommendedProxySettings = false; # manual control
                extraConfig = ''
                  proxy_set_header Host $host;
                  proxy_set_header X-Forwarded-For $remote_addr;
                  proxy_set_header X-Forwarded-Proto https;

                  proxy_read_timeout 300s;
                  proxy_send_timeout 300s;

                  # federation and sync spam
                  access_log off;
                '';
              };
            })
            [
              "/_matrix"
              "/_continuwuity"
              "/.well-known/matrix"
            ]
        );
      };
    }
    # rocksdb archives wals it never reads, on btrfs each one pins its whole fallocated extent
    {
      systemd.services.continuwuity-wal-archive-clean = {
        description = "Remove archived continuwuity WAL files";
        serviceConfig.Type = "oneshot";
        script = ''
          dir=/var/lib/private/continuwuity/archive
          [ -d "$dir" ] || exit 0
          ${getExe' pkgs.findutils "find"} "$dir" -maxdepth 1 -type f -name '*.log' -mmin +10 -delete
        '';
      };

      systemd.timers.continuwuity-wal-archive-clean = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnBootSec = "5min";
          OnUnitActiveSec = "10min";
        };
      };
    }
    # kanidm sso, legacy password login goes away with this
    (
      let
        idm = infra.kanidm.domain;
        secret = "kanidm_continuwuity";
      in
      {
        meow.sops.enableSecrets = [ secret ];
        sops.secrets.${secret}.restartUnits = [ "continuwuity.service" ];

        # the secret belongs to kanidm
        systemd.services.continuwuity.serviceConfig.LoadCredential = [
          "kanidm:${config.sops.secrets.${secret}.path}"
        ];

        services.matrix-continuwuity.settings.global.oauth.oidc = {
          discovery_url = "https://${idm}/oauth2/openid/continuwuity";
          client_id = "continuwuity";
          client_secret_file = "/run/credentials/continuwuity.service/kanidm";
          additional_scopes = [
            "profile"
            "email"
          ];
          provider_name = "Kanidm";
          provider_icon = "https://${idm}/pkg/img/logo-square.svg";
          profile_key_map.displayname = "name";
        };
      }
    )
    # livekit (matrixrtc and element call)
    {
      # only the media ports, signalling goes through nginx
      networking.firewall.allowedUDPPortRanges = with config.services.livekit.settings.rtc; [
        {
          from = port_range_start;
          to = port_range_end;
        }
      ];

      services.livekit = {
        enable = true;
        settings = {
          room.auto_create = false;
          rtc.enable_loopback_candidate = false;

          rtc.use_external_ip = true;
          rtc.port_range_start = 50001;
          rtc.port_range_end = 51001;
        };
        keyFile = "/run/livekit.key";
      };

      services.lk-jwt-service = {
        enable = true;
        port = 8084;
        livekitUrl = "wss://${cfg.domain}/livekit";
        inherit (config.services.livekit) keyFile;
      };

      systemd.services.livekit = {
        after = [ "generate-livekit-key.service" ];
        requires = [ "generate-livekit-key.service" ];
      };

      systemd.services.lk-jwt-service = {
        after = [ "generate-livekit-key.service" ];
        requires = [ "generate-livekit-key.service" ];
        environment.LIVEKIT_FULL_ACCESS_HOMESERVERS = cfg.domain;
      };

      systemd.services.generate-livekit-key = {
        before = [
          "lk-jwt-service.service"
          "livekit.service"
        ];
        wantedBy = [ "multi-user.target" ];
        path = with pkgs; [
          livekit
          coreutils
          gawk
        ];
        script = ''
          if [ ! -s "${config.services.livekit.keyFile}" ]; then
            echo "Key missing, generating key"
            echo "lk-jwt-service: $(livekit-server generate-keys | tail -1 | awk '{print $3}')" > "${config.services.livekit.keyFile}"
          else
            chmod 0600 "${config.services.livekit.keyFile}"
          fi
        '';
        serviceConfig = {
          Type = "oneshot";
          UMask = "0077";
        };
      };

      services.matrix-continuwuity.settings.global.matrix_rtc.foci = [
        {
          type = "livekit";
          livekit_service_url = "https://${cfg.domain}/livekit";
        }
      ];

      services.nginx.virtualHosts."${cfg.domain}".locations = {
        "~ ^/livekit/(sfu/get|get_token|healthz)" = {
          priority = 400;
          proxyPass = "http://[::1]:${toString config.services.lk-jwt-service.port}";
          # sets Host, a second one makes livekit/go reply 400
          recommendedProxySettings = true;
          extraConfig = ''
            rewrite ^/livekit/(.*)$ /$1 break;
          '';
        };

        "/livekit/" = {
          priority = 401;
          proxyPass = "http://[::1]:${toString config.services.livekit.settings.port}/";
          proxyWebsockets = true;
          recommendedProxySettings = true;
          extraConfig = ''
            proxy_send_timeout 300;
            proxy_read_timeout 300;
            proxy_buffering off;

            proxy_set_header Accept-Encoding gzip;
          '';
        };
      };
    }
    # livekit coturn
    (mkIf config.meow.server.coturn {
      services.livekit.settings.rtc.turn_servers = [
        {
          host = config.services.coturn.realm;
          port = 5349;
          protocol = "tls";
          secret = "";
        }
      ];

      systemd.services.livekit = {
        serviceConfig.RuntimeDirectory = "livekit";
        serviceConfig.LoadCredential = [ "turn-secret:${config.sops.secrets.coturn_secret.path}" ];

        # inject coturn secret at startup
        preStart = ''
          ${getExe pkgs.jq} \
            --rawfile sec "$CREDENTIALS_DIRECTORY/turn-secret" \
            '.rtc.turn_servers[0].secret = ($sec | rtrimstr("\n"))' \
            ${(pkgs.formats.json { }).generate "livekit.json" config.services.livekit.settings} \
            > "$RUNTIME_DIRECTORY/livekit.json"
        '';

        serviceConfig.ExecStart = mkForce "${getExe pkgs.livekit} --config=\${RUNTIME_DIRECTORY}/livekit.json --key-file \${CREDENTIALS_DIRECTORY}/livekit-secrets";
      };
    })
    # web interface
    {
      services.nginx.virtualHosts."matrix.${cfg.domain}" = {
        root = pkgs.sable;

        locations."/".extraConfig = ''
          rewrite ^/config.json$ /config.json break;
          rewrite ^/manifest.json$ /manifest.json break;

          rewrite ^/sw.js$ /sw.js break;
          rewrite ^/pdf.worker.min.js$ /pdf.worker.min.js break;

          rewrite ^/public/(.*)$ /public/$1 break;
          rewrite ^/assets/(.*)$ /assets/$1 break;

          rewrite ^(.+)$ /index.html break;
        '';
      };
    }
  ]);
}
