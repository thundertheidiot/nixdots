{
  inputs,
  lib,
  mlib,
  config,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib) mkIf head;
  inherit (lib.types) listOf nullOr str;

  cfg = config.meow.server;
in {
  imports = [
    inputs.nixos-mailserver.nixosModules.default
  ];

  options.meow.server.mail.domains = mkOpt (listOf str) [] {};
  options.meow.server.mail.fqdn = mkOpt (nullOr str) null {};

  config = mkIf cfg.mail {
    meow.impermanence.directories = [
      {path = "/var/lib/postgresql";}
      {path = "/var/lib/dovecot";}
      {path = "/var/lib/postfix";}
      {path = "/var/lib/opendkim";}
      {path = "/var/spool/mail";}
      {path = "/var/cache/knot-resolver";}
      {path = "/var/lib/rspamd";}
    ];

    mailserver = let
      p = dir: "${config.meow.impermanence.persist}/${dir}";
    in {
      enable = true;

      sieveDirectory = p "mail/sieve";
      mailDirectory = p "mail/vmail";
      dkimKeyDirectory = p "mail/dkim";
      backup.snapshotRoot = p "mail/rsnapshot";
      indexDir = p "mail/index";

      fqdn = cfg.mail.fqdn or "mail.${head cfg.mailDomains}";
      inherit (cfg.mail) domains;

      loginAccounts = {
        # nix-shell -p mkpasswd --run "mkpasswd -sm bcrypt"
        "perkele@saatana.xyz" = {
          hashedPassword = "$2b$05$7GdXl3NnetmS8yW4SL8UEuyhjtqDrkwk8r7sup8khTQZmcDcY8n7e";
        };
      };

      certificateScheme = "acme-nginx";
    };

    services.roundcube = {
      enable = true;
      configureNginx = true;
      hostName = config.mailserver.fqdn;
      extraConfig = ''
        $config['smtp_server'] = "tls://${config.mailserver.fqdn}";
        $config['smtp_user'] = "%u";
        $config['smtp_pass'] = "%p";
      '';
    };
  };
}
