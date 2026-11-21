{
  config,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf str;
  inherit (lib)
    mkIf
    mkMerge
    length
    unique
    listToAttrs
    mkDefault
    ;

  cfg = config.meow.server;
in
{
  options.meow.server.certificates = mkOpt (listOf str) [ ] { };

  config = mkMerge [
    (mkIf (length cfg.certificates > 0 || config.meow.dns.enable) {
      security.acme = {
        acceptTerms = true;
        defaults = {
          email = "thundertheidiot@proton.me";
          group = "acme";
        };

        # staging environment
        # server = "https://acme-staging-v02.api.letsencrypt.org/directory";
      };

      users.users."${config.services.nginx.user}".extraGroups = [ "acme" ];

      meow.impermanence.directories = [
        { path = "/var/lib/acme"; permissions = "0755"; user = "acme"; group = "acme"; }
      ];
    })

    (mkIf (length cfg.certificates > 0) {
      security.acme.certs = listToAttrs (
        map (name: {
          inherit name;
          value = {
            group = "acme";
          };
        }) (unique cfg.certificates)
      );

      services.nginx.virtualHosts = listToAttrs (
        map (name: {
          inherit name;
          value = {
            forceSSL = mkDefault true;
            enableACME = mkDefault true;
          };
        }) (unique cfg.certificates)
      );
    })
  ];
}
