{
  config,
  infra,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) genAttrs head mkIf;

  cfg = config.meow.server;
  inherit (cfg.services) vaultwarden;
in
{
  options.meow.server.vaultwarden = mkEnOpt "Vaultwarden";

  config = mkIf cfg.vaultwarden {
    meow.server.services.vaultwarden = {
      internal = null;
      port = 8222;
      expose = [
        "vw.kotiboksi.xyz"
        "vw.meowcloud.net"
      ];
    };

    # admin panel only over wireguard
    services.nginx.virtualHosts = genAttrs vaultwarden.expose (_: {
      locations."/admin" = {
        proxyPass = "http://127.0.0.1:${toString vaultwarden.port}";
        recommendedProxySettings = true;
        extraConfig = ''
          allow ${infra.networks.wg.cidr};
          deny all;
        '';
      };
    });

    meow.impermanence.directories = [
      {
        path = "/var/lib/vaultwarden";
        permissions = "0700";
        user = config.systemd.services.vaultwarden.serviceConfig.User;
        group = config.systemd.services.vaultwarden.serviceConfig.Group;
      }
      {
        path = "/var/backup/vaultwarden";
        permissions = "0770";
        user = config.systemd.services.vaultwarden.serviceConfig.User;
        group = config.systemd.services.vaultwarden.serviceConfig.Group;
      }
    ];

    services.vaultwarden = {
      enable = true;
      environmentFile = config.sops.secrets."vaultwarden_env".path;
      backupDir = "/var/backup/vaultwarden";

      config = {
        DOMAIN = "https://${head vaultwarden.expose}";

        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = vaultwarden.port;

        SIGNUPS_ALLOWED = false;

        PUSH_ENABLED = true;
        PUSH_RELAY_URI = "https://api.bitwarden.eu";
        PUSH_IDENTITY_URI = "https://identity.bitwarden.eu";
      };
    };
  };
}
