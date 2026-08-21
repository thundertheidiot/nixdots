{
  lib,
  mlib,
  pkgs,
  config,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server.matrix;
in {
  options = {
    meow.server.matrix.whatsapp.enable = mkEnOpt "Mautrix Whatsapp";
  };

  config = mkIf (cfg.enable && cfg.whatsapp.enable) {
    meow.impermanence.directories = [
      {
        path = "/var/lib/mautrix-whatsapp";
        user = "mautrix-whatsapp";
        group = "mautrix-whatsapp";
      }
    ];

    # :(
    meow.permittedInsecurePackages = [
      "olm-3.2.16"
    ];

    sops.secrets."mautrix_whatsapp_env" = {
      owner = "mautrix-whatsapp";
      group = "mautrix-whatsapp";
    };

    services.postgresql = {
      enable = true;

      ensureDatabases = ["mautrix-whatsapp"];
      ensureUsers = [
        {
          name = "mautrix-whatsapp";
          ensureDBOwnership = true;
        }
      ];
    };

    services.mautrix-whatsapp = {
      enable = true;
      environmentFile = config.sops.secrets."mautrix_whatsapp_env".path;
      serviceDependencies = ["continuwuity.service"];

      settings = {
        homeserver.address = "http://127.0.0.1:8008";
        homeserver.domain = cfg.domain;

        network.displayname_template = "{{or .PushName .FullName .BusinessName .Phone}} (WA)";

        database.type = "postgres";
        database.uri = "postgres:///mautrix-whatsapp?host=/var/run/postgresql";

        bridge = {
          permissions = {
            "meowcloud.net" = "user";
            "@thunder:meowcloud.net" = "admin";
          };
        };

        backfill = {
          enabled = true;
          max_initial_messages = 50;
        };

        encryption = {
          allow = true;
          default = true;
          require = true;

          pickle_key = "$PICKLE_KEY";
        };
      };
    };
  };
}
