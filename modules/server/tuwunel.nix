{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib.types) str;
  inherit (lib) mkIf mkMerge listToAttrs head mkForce getExe;

  cfg = config.meow.server.matrix;
in {
  options = {
    meow.server.matrix.enable = mkEnOpt "Matrix";
    meow.server.matrix.domain = mkOpt str config.meow.server.mainDomain {};
  };

  config = mkIf cfg.enable (mkMerge [
    {
      meow.impermanence.directories = [
        "/var/lib/private/tuwunel"
        {
          path = config.services.matrix-tuwunel.settings.global.database_backup_path;
          inherit (config.services.matrix-tuwunel) user group;
        }
      ];

      meow.server.certificates = [
        "matrix.${cfg.domain}" # web ui
        cfg.domain
      ];

      sops.secrets."matrix_registration_token" = {
        owner = config.services.matrix-tuwunel.user;
        group = config.services.matrix-tuwunel.group;
        mode = "0440";
      };

      users.users."${config.services.matrix-tuwunel.user}".extraGroups = ["turnserver"];

      services.matrix-tuwunel = {
        enable = true;
        settings = mkMerge [
          {
            global = {
              server_name = cfg.domain;
              port = [8008];
              address = ["127.0.0.1" "::1"];
              database_backup_path = "/opt/tuwunel-backups";

              allow_registration = true;
              registration_token_file = config.sops.secrets."matrix_registration_token".path;

              allow_federation = true;
              trusted_servers = ["matrix.org"];

              well_known = {
                client = "https://${cfg.domain}";
                server = "${cfg.domain}:443";
              };
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
        locations = listToAttrs (map (name: {
          inherit name;
          value = {
            proxyPass = "http://127.0.0.1:${toString (head config.services.matrix-tuwunel.settings.global.port)}";
            recommendedProxySettings = false; # manual control
            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Forwarded-For $remote_addr;
              proxy_set_header X-Forwarded-Proto https;

              proxy_read_timeout 300s;
              proxy_send_timeout 300s;
            '';
          };
        }) ["/_matrix" "/_tuwunel" "/.well-known/matrix"]);
      };
    }
    # livekit (matrixrtc and element call)
    {
      services.livekit = {
        enable = true;
        openFirewall = true;
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

      systemd.services.lk-jwt-service.environment.LIVEKIT_FULL_ACCESS_HOMESERVERS = cfg.domain;

      systemd.services.generate-livekit-key = {
        before = ["lk-jwt-service.service" "livekit.service"];
        wantedBy = ["multi-user.target"];
        path = with pkgs; [livekit coreutils gawk];
        script = ''
          echo "Key missing, generating key"
          echo "lk-jwt-service: $(livekit-server generate-keys | tail -1 | awk '{print $3}')" > "${config.services.livekit.keyFile}"

        '';
        serviceConfig.Type = "oneshot";
        unitConfig.ConditionPathExists = "!${config.services.livekit.keyFile}";
      };

      services.matrix-tuwunel.settings.global.well_known.rtc_transports = [
        {
          type = "livekit";
          livekit_service_url = "https://${cfg.domain}/livekit";
        }
      ];

      services.nginx.virtualHosts."${cfg.domain}".locations = {
        "~ ^/livekit/(jwt|sfu/get|get_token|healthz)" = {
          priority = 400;
          proxyPass = "http://[::1]:${toString config.services.lk-jwt-service.port}";
          extraConfig = ''
            rewrite ^/livekit/(.*)$ /$1 break;
            proxy_set_header Host $host;
          '';
        };

        "/livekit/" = {
          priority = 401;
          proxyPass = "http://[::1]:${toString config.services.livekit.settings.port}/";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_send_timeout 300;
            proxy_read_timeout 300;
            proxy_buffering off;

            proxy_set_header Accept-Encoding gzip;
            proxy_set_header Host $host;
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
        serviceConfig.LoadCredential = ["turn-secret:${config.sops.secrets.coturn_secret.path}"];

        # inject coturn secret at startup
        preStart = ''
          ${getExe pkgs.jq} \
            --arg sec "$(cat "$CREDENTIALS_DIRECTORY/turn-secret")" \
            '.rtc.turn_servers[0].secret = $sec' \
            ${(pkgs.formats.json {}).generate "livekit.json" config.services.livekit.settings} \
            > "$RUNTIME_DIRECTORY/livekit.json"
        '';

        serviceConfig.ExecStart =
          mkForce
          ''${getExe pkgs.livekit} --config=''${RUNTIME_DIRECTORY}/livekit.json --key-file ''${CREDENTIALS_DIRECTORY}/livekit-secrets'';
      };
    })
    # web interface
    {
      services.nginx.virtualHosts."matrix.${cfg.domain}" = {
        root = pkgs.mpkgs.sable;

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
