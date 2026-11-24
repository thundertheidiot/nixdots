{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkOverride;
  rss = config.meow.dns.names.freshrss;

  # intl only adds idn feed urls and drags in its own icu
  php = pkgs.php.withExtensions (
    { enabled, ... }: builtins.filter (e: e.extensionName != "intl") enabled
  );
in
{
  config = {
    meow.server.services.freshrss = {
      internal = "rss";
      access = "wg";
    };

    meow.impermanence.directories = [
      {
        path = "/var/lib/freshrss";
        user = config.services.freshrss.user;
        group = config.users.users.${config.services.freshrss.user}.group;
        permissions = "0750";
      }
      {
        path = "/var/lib/postgresql";
        user = "postgres";
        group = "postgres";
        permissions = "0700";
      }
    ];

    services.postgresql = {
      enable = true;
      ensureDatabases = [ config.services.freshrss.database.name ];

      authentication = mkOverride 10 ''
        #type database  DBuser  auth-method
        local all postgres peer
        local sameuser all peer
        host sameuser all 127.0.0.1/32 scram-sha-256
        host sameuser all ::1/128 scram-sha-256
        host all freshrss 127.0.0.1/32 scram-sha-256
        host all freshrss ::1/128 scram-sha-256
      '';

      ensureUsers = [
        {
          name = config.services.freshrss.database.user;
          ensureDBOwnership = true;
          ensureClauses = {
            login = true;
            createdb = true;
            password = "SCRAM-SHA-256$4096:0Zm8D/Y7Znv/RGUeTgUpbw==$jgsawXkcIQWhY/pxhm7jLwuLWmepPRi/lJtW7Q6TcZQ=:btfodZnT0q8IfZCCM28nwUBnyPOYXH6i9St8kTGcYK8=";
          };
        }
      ];
    };

    sops.secrets.freshrss_db_password.owner = "freshrss";

    services.phpfpm.pools.freshrss.phpPackage = php;

    services.freshrss = {
      enable = true;
      package = pkgs.freshrss.override { inherit php; };
      baseUrl = "https://${rss}";
      virtualHost = rss;

      passwordFile = config.sops.secrets.freshrss_db_password.path;

      defaultUser = "thunder";
      api.enable = true;

      webserver = "nginx";

      database = {
        host = "/run/postgresql";
        port = null;
        type = "pgsql";
        passFile = config.sops.secrets.freshrss_db_password.path;

        name = "freshrss";
        user = "freshrss";
      };
    };
  };
}
