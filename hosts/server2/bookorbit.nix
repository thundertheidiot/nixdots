{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe getExe' mkAfter;
  inherit (config.meow.dns) names;

  port = 5066;
  dataDir = "${config.meow.impermanence.persist}/bookorbit";
  booksDir = "${dataDir}/books";
  url = "https://${names.bookorbit}";
in
{
  config = {
    meow.server.services.bookorbit.internal = "books";

    services.nginx.virtualHosts.${names.bookorbit} = {
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString port}";
        recommendedProxySettings = true;
        proxyWebsockets = true;
        extraConfig = ''
          client_max_body_size 512m;
        '';
      };
    };

    services.postgresql = {
      ensureDatabases = [ "bookorbit" ];
      ensureUsers = [
        {
          name = "bookorbit";
          ensureDBOwnership = true;
        }
      ];
      extensions = ps: [ ps.pgvector ];
    };

    # creating vector needs a superuser, the setup unit runs as postgres
    systemd.services.postgresql-setup.script = mkAfter ''
      psql -d bookorbit -tAc 'CREATE EXTENSION IF NOT EXISTS vector'
    '';

    users.groups.bookorbit = { };
    users.users.bookorbit = {
      isSystemUser = true;
      group = "bookorbit";
    };

    systemd.tmpfiles.rules = [
      "d ${dataDir} 0700 bookorbit bookorbit -"
      "d ${dataDir}/dock 0700 bookorbit bookorbit -"
      "d ${booksDir} 0750 bookorbit bookorbit -"
    ];

    systemd.services.bookorbit = {
      description = "BookOrbit";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network-online.target"
        "postgresql-setup.service"
      ];
      wants = [ "network-online.target" ];
      requires = [ "postgresql-setup.service" ];
      unitConfig.RequiresMountsFor = [ dataDir ];
      environment = {
        NODE_ENV = "production";
        DATABASE_URL = "postgres://bookorbit@/bookorbit?host=/run/postgresql";
        APP_DATA_PATH = dataDir;
        BOOK_DOCK_PATH = "${dataDir}/dock";
        LIBRARY_BROWSE_ROOT = booksDir;
        HOST = "127.0.0.1";
        PORT = toString port;
        APP_URL = url;
        CLIENT_URL = url;
        TRUST_PROXY = "true";
        HOME = "/run/bookorbit";
      };
      serviceConfig = {
        User = "bookorbit";
        Group = "bookorbit";
        UMask = "0002";
        RuntimeDirectory = "bookorbit";
        EnvironmentFile = config.sops.secrets.bookorbit_env.path;
        ExecStartPre = getExe' pkgs.bookorbit "bookorbit-migrate";
        ExecStart = getExe pkgs.bookorbit;
        Restart = "on-failure";
        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        RestrictRealtime = true;
        LockPersonality = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        ReadWritePaths = [ dataDir ];
        InaccessiblePaths = [
          "/run/secrets"
          "-/run/secrets.d"
        ];
      };
    };
  };
}
