{
  config,
  pkgs,
  ...
}:
let
  dataDir = "/nix/persist/kissasome";
  inherit (config.meow.server.services.kissasome) port;
in
{
  config = {
    meow.server.services.kissasome = {
      internal = null;
      port = 23817;
      expose = [ "kissasome.meowcloud.net" ];
    };

    users.users.kissasome = {
      isSystemUser = true;
      group = "kissasome";
    };
    users.groups.kissasome = { };

    systemd.tmpfiles.settings."10-kissasome" = {
      ${dataDir}.d = {
        user = "kissasome";
        group = "kissasome";
        mode = "0750";
      };
      "${dataDir}/images".d = {
        user = "kissasome";
        group = "kissasome";
        mode = "0750";
      };
    };

    systemd.services.kissasome = {
      description = "kissasome";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];

      environment = {
        DATABASE_URL = "sqlite://${dataDir}/db.sqlite?mode=rwc";
        IMAGE_PATH = "${dataDir}/images";
        PORT = toString port;
      };

      serviceConfig = {
        ExecStart = "${pkgs.kissasome}/bin/kissasome";
        EnvironmentFile = config.sops.secrets.kissasome_env.path;
        WorkingDirectory = dataDir;
        User = "kissasome";
        Group = "kissasome";
        Restart = "on-failure";

        ReadWritePaths = [ dataDir ];
        UMask = "0077";

        NoNewPrivileges = true;
        CapabilityBoundingSet = "";
        AmbientCapabilities = "";
        PrivateTmp = true;
        PrivateDevices = true;
        PrivateUsers = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectProc = "invisible";
        ProcSubset = "pid";
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectKernelLogs = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
          "~@resources"
        ];
      };
    };
  };
}
