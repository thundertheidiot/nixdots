{
  pkgs,
  lib,
  mlib,
  config,
  ...
}: let
  inherit (mlib) mkOpt mkEnOpt;
  inherit (lib) mkIf;
  inherit (lib.types) nullOr str;

  cfg = config.meow.server.forgejo;
in {
  options.meow.server.forgejo = {
    enable = mkEnOpt "Enable forgejo";
    domain = mkOpt (nullOr str) null {};
  };

  config = mkIf cfg.enable {
    meow.impermanence.directories = [
      {
        path = "/var/lib/forgejo";
        persistPath = "${config.meow.impermanence.persist}/forgejo";
        user = "forgejo";
        group = "forgejo";
      }
      {path = "/var/lib/postgresql";}
    ];

    meow.server.reverseProxy = {
      "${cfg.domain}" = "http://127.0.0.1:${toString config.services.forgejo.settings.server.HTTP_PORT}";
    };

    services.forgejo = {
      enable = true;
      lfs.enable = true;
      dump.enable = true;

      settings = {
        server = {
          ROOT_URL = "https://${cfg.domain}";
          HTTP_PORT = 3001;
        };

        "repository.upload" = {
          FILE_MAX_SIZE = 256;
        };

        quota = {
          ENABLED = true;
        };

        "quota.default".TOTAL = "10G";

        service.DISABLE_REGISTRATION = true;
      };

      database.type = "postgres";
      database.passwordFile = "${config.sops.secrets."forgejo_postgres".path}";
    };
  };
}
