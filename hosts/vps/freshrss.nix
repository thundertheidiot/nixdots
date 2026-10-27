{
  config,
  lib,
  inputs,
  ...
}:
{
  imports = [ (import ../../certs).module ];

  config = {
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

      authentication = lib.mkOverride 10 ''
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

    services.nginx.virtualHosts."rss.vps.home" = {
      onlySSL = true;
      listen = [
        {
          addr = (import "${inputs.self.outPath}/sops/wireguard").peers.vps.address;
          port = 443;
          ssl = true;
        }
      ];
      sslCertificate = (import "${inputs.self.outPath}/certs")."local.crt";
      sslCertificateKey = config.sops.secrets.localKey.path;
    };

    services.freshrss = {
      enable = true;
      baseUrl = "https://rss.vps.home";
      virtualHost = "rss.vps.home";

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
