{
  config,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf str;
  inherit (lib) mkIf length unique listToAttrs;

  cfg = config.meow.server;
in {
  options.meow.server.certificates = mkOpt (listOf str) [] {};

  config = mkIf (length cfg.certificates > 0) {
    security.acme = {
      acceptTerms = true;
      defaults = {
        email = "thundertheidiot@proton.me";
        group = "acme";
      };

      # staging environment
      # server = "https://acme-staging-v02.api.letsencrypt.org/directory";
    };

    users.users."${config.services.nginx.user}".extraGroups = ["acme"];

    security.acme.certs = listToAttrs (map (name: {
        inherit name;
        value = {
          group = "acme";
        };
      })
      (unique cfg.certificates));

    services.nginx.virtualHosts = listToAttrs (map (name: {
        inherit name;
        value = {
          forceSSL = true;
          enableACME = true;
        };
      })
      (unique cfg.certificates));

    meow.impermanence.directories = [
      {path = "/var/lib/acme";}
    ];
  };
}
