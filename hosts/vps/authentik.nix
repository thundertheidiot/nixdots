{
  lib,
  config,
  ...
}: {
  meow.server.domains = ["auth.kotiboksi.xyz"];

  services.authentik = {
    enable = true;
    environmentFile = config.sops.secrets."authentik_env".path;

    nginx = {
      enable = true;
      enableACME = true;
      host = "auth.kotiboksi.xyz";
    };

    settings = {
      disable_startup_analytics = true;
      avatars = "initials";

      email = {
        host = "localhost";
        port = 25;
        from = "noreply@local";
      };
    };
  };
}
