{
  config,
  pkgs,
  ...
}:
{
  meow.impermanence.directories = [
    {
      path = "/var/lib/kotiboksi";
      user = "kotiboksi";
      group = "kotiboksi";
      permissions = "0700";
    }
  ];

  users.users.kotiboksi = {
    isSystemUser = true;
    group = "kotiboksi";
  };
  users.groups.kotiboksi = { };

  # Migrate existing root-owned database files, not just the persistence directory.
  systemd.tmpfiles.rules = [ "Z /var/lib/kotiboksi - kotiboksi kotiboksi - -" ];

  meow.server.reverseProxy = {
    "${config.meow.server.mainDomain}" = "http://127.0.0.1:3005";
    "thunder.meowcloud.net" = "http://127.0.0.1:3005";
  };

  meow.server.radio.domains = [
    config.meow.server.mainDomain
    "thunder.meowcloud.net"
  ];

  systemd.services."leptos-kotiboksi" = {
    enable = true;
    description = "Leptos Website";

    environment = {
      DATABASE_URL = "/var/lib/kotiboksi/guestbook.db";
      LEPTOS_SITE_ADDR = "127.0.0.1:3005";
    };

    serviceConfig = {
      Type = "simple";
      User = "kotiboksi";
      Group = "kotiboksi";
      ExecStart = "${pkgs.leptos-kotiboksi}/bin/kotiboksi";
      WorkingDirectory = "/var/lib/kotiboksi";
      StateDirectory = "kotiboksi";
      StateDirectoryMode = "0700";
      UMask = "0077";
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
      RestrictNamespaces = true;
      RestrictRealtime = true;
      LockPersonality = true;
      RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
    };
    wantedBy = [ "multi-user.target" ];
  };
}
