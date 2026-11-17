{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf filterAttrs mapAttrs;

  cfg = config.meow.server;
  url = "http://127.0.0.1:${toString config.services.vaultwarden.config.ROCKET_PORT}";
in
{
  options.meow.server.vaultwarden = mkEnOpt "Vaultwarden";

  config = mkIf cfg.vaultwarden {
    meow.server.reverseProxy."vw.${cfg.mainDomain}" = url;

    # admin panel only over wireguard, on every vhost proxying vaultwarden
    services.nginx.virtualHosts = mapAttrs (_: _: {
      locations."/admin" = {
        proxyPass = url;
        recommendedProxySettings = true;
        extraConfig = ''
          allow 10.100.0.0/24;
          deny all;
        '';
      };
    }) (filterAttrs (_: target: target == url) cfg.reverseProxy);

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
        DOMAIN = "https://vw.${cfg.mainDomain}";

        ROCKET_ADDRESS = "127.0.0.1";
        ROCKET_PORT = 8222;

        SIGNUPS_ALLOWED = false;

        PUSH_ENABLED = true;
        PUSH_RELAY_URI = "https://api.bitwarden.eu";
        PUSH_IDENTITY_URI = "https://identity.bitwarden.eu";
      };
    };
  };
}
