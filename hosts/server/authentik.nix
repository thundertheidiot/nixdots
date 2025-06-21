{
  lib,
  config,
  ...
}: {
  config = {
    sops.secrets."authentik_env".mode = "0644";

    meow.impermanence.directories = [
      {
        path = "/var/lib/postgresql";
      }
    ];

    server.domains = [
      "auth.home"
    ];

    services.nginx.virtualHosts."auth.home" = let
      certs = import ../../certs;
    in {
      forceSSL = lib.mkForce true;

      sslCertificate = certs."local.crt";
      sslCertificateKey = config.sops.secrets.localKey.path;
    };

    services.authentik = {
      enable = false;
      environmentFile = config.sops.secrets."authentik_env".path;

      nginx = {
        enable = false;
        enableACME = false;
        host = "auth.local";
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
  };
}
