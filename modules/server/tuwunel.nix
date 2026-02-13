{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib.types) str;
  inherit (lib) mkIf mkMerge listToAttrs head;

  cfg = config.meow.server.matrix;
in {
  options = {
    meow.server.matrix.enable = mkEnOpt "Matrix";
    meow.server.matrix.domain = mkOpt str config.meow.server.mainDomain {};
  };

  config = mkIf cfg.enable {
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

            trusted_servers = ["matrix.org"];

            well-known = {
              client = "https://matrix.${cfg.domain}";
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

    networking.firewall.allowedTCPPorts = [8448];

    services.nginx.clientMaxBodySize = "60M";
    services.nginx.virtualHosts."${cfg.domain}" = {
      listen = [
        {
          addr = "0.0.0.0";
          port = 80;
        }
        {
          addr = "[::]";
          port = 80;
        }
        {
          addr = "0.0.0.0";
          port = 443;
          ssl = true;
        }
        {
          addr = "[::]";
          port = 443;
          ssl = true;
        }
        {
          addr = "0.0.0.0";
          port = 8448;
          ssl = true;
        }
        {
          addr = "[::]";
          port = 8448;
          ssl = true;
        }
      ];

      forceSSL = true;

      locations = listToAttrs (map (name: {
        inherit name;
        value = {
          proxyPass = "http://127.0.0.1:${toString (head config.services.matrix-tuwunel.settings.global.port)}${name}";
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

    # web interface
    services.nginx.virtualHosts."matrix.${cfg.domain}".root = pkgs.cinny;
  };
}
