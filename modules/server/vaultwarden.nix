{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.server;
in {
  options.meow.server.vaultwarden = mkEnOpt "Vaultwarden";

  config = mkIf cfg.vaultwarden {
    meow.server.reverseProxy = {
      "vw.${cfg.mainDomain}" = "http://127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}";
    };

    meow.impermanence.directories = [
      {
        path = "/var/lib/vaultwarden";
        user = config.systemd.services.vaultwarden.serviceConfig.User;
        group = config.systemd.services.vaultwarden.serviceConfig.Group;
      }
      {
        path = "/var/backup/vaultwarden";
        user = config.systemd.services.vaultwarden.serviceConfig.User;
        group = config.systemd.services.vaultwarden.serviceConfig.Group;
      }
    ];

    services.vaultwarden = {
      enable = true;
      environmentFile = config.sops.secrets."vaultwarden_env".path;
      backupDir = "/var/backup/vaultwarden";

      config = {
        DOMAIN = "https://vw.${cfg.mainDomain}";

        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8222;

        PUSH_ENABLED = true;
        PUSH_RELAY_URI = "https://api.bitwarden.eu";
        PUSH_IDENTITY_URI = "https://identity.bitwarden.eu";
      };
    };
  };
}
