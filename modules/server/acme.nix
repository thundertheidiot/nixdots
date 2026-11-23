{
  config,
  infra,
  lib,
  mlib,
  ...
}:
let
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf str;
  inherit (lib)
    genAttrs
    mkDefault
    mkIf
    mkMerge
    unique
    ;

  cfg = config.meow.server;
in
{
  options.meow.server = {
    certificates = mkOpt (listOf str) [ ] { };
    dnsCertificates = mkOpt (listOf str) [ ] {
      description = "certificates validated through porkbun dns, for names that http can't reach";
    };
  };

  config = mkMerge [
    (mkIf (cfg.certificates != [ ] || cfg.dnsCertificates != [ ]) {
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
        {
          path = "/var/lib/acme";
          permissions = "0755";
          user = "acme";
          group = "acme";
        }
      ];
    })

    (mkIf (cfg.certificates != [ ]) {
      security.acme.certs = genAttrs (unique cfg.certificates) (_: {
        group = "acme";
      });

      services.nginx.virtualHosts = genAttrs (unique cfg.certificates) (_: {
        forceSSL = mkDefault true;
        enableACME = mkDefault true;
      });
    })

    (mkIf (cfg.dnsCertificates != [ ]) {
      sops.secrets.porkbun = {
        sopsFile = infra.secrets.porkbun;
        format = "dotenv";
      };

      security.acme.certs = genAttrs cfg.dnsCertificates (_: {
        dnsProvider = "porkbun";
        # the local dnsmasq shadows internal names
        dnsResolver = "1.1.1.1:53";
        environmentFile = config.sops.secrets.porkbun.path;
      });
    })
  ];
}
