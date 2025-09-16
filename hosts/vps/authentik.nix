{
  lib,
  config,
  ...
}: {
  meow.impermanence.directories = [
    {
      path = "/var/lib/postgresql";
    }
    {
      path = "/var/lib/authentik";
    }
  ];

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
