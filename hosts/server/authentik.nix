{config, ...}: {
  config = {
    meow.sops.enableSecrets = [
      "server_authentik_env"
    ];

    meow.sops.secrets."server_authentik_env".mode = "0644";

    services.authentik = {
      enable = true;
      environmentFile = config.sops.secrets."server_authentik_env".path;

      settings = {
        disable_startup_analytics = true;
        avatars = "initials";
      };
    };
  };
}
