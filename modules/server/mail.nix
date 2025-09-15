{
  inputs,
  lib,
  mlib,
  config,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib) mkIf head length;
  inherit (lib.types) listOf nullOr str;

  cfg = config.meow.server;
in {
  imports = [
    inputs.nixos-mailserver.nixosModules.default
  ];

  options.meow.server.mail.domains = mkOpt (listOf str) [] {};
  options.meow.server.mail.fqdn = mkOpt (nullOr str) null {};

  config = mkIf (length cfg.mail.domains > 0) {
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

      fqdn =
        if cfg.mail.fqdn != null
        then cfg.mail.fqdn
        else "mail.${head cfg.mail.domains}";
      inherit (cfg.mail) domains;

      # nix-shell -p mkpasswd --run "mkpasswd -sm bcrypt"

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
