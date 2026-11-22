{
  config,
  infra,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) head mkIf filterAttrs mapAttrs;

  cfg = config.meow.server;
  inherit (infra.services) vaultwarden;
  url = "http://127.0.0.1:${toString vaultwarden.port}";
in
{
  options.meow.server.vaultwarden = mkEnOpt "Vaultwarden";

  config = mkIf cfg.vaultwarden {
    # admin panel only over wireguard, on every vhost proxying vaultwarden
    services.nginx.virtualHosts = mapAttrs (_: _: {
      locations."/admin" = {
        proxyPass = url;
        recommendedProxySettings = true;
        extraConfig = ''
          allow ${infra.networks.wg.cidr};
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
        DOMAIN = "https://${head vaultwarden.public}";

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
