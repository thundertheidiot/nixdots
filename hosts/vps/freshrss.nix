{
  config,
  lib,
  ...
}: {
  config = {
    meow.impermanence.directories = [
      {
        path = "/var/lib/freshrss";
      }
      {
        path = "/var/lib/postgresql";
      }
    ];

    services.postgresql = {
      enable = true;
      ensureDatabases = [config.services.freshrss.database.name];

      authentication = lib.mkOverride 10 ''
        #type database  DBuser  auth-method
        local sameuser all trust
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

    services.nginx.virtualHosts."rss.meowcloud.net" = {
      forceSSL = true;
      enableACME = true;
    };

    services.freshrss = {
      enable = true;
      baseUrl = "https://rss.meowcloud.net";
      virtualHost = "rss.meowcloud.net";

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
