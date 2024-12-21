{
  config,
  pkgs,
  ...
}: {
  meow.impermanence.ensureDirectories = [
    "/persist/media/downloads/incomplete/torrents"
    "/persist/media/downloads/torrents"
  ];

  meow.impermanence.directories = [
    {
      path = "/var/lib/qbittorrent";
      persistPath = "/persist/media_stack_data/qbittorrent";
      user = "qbittorrent";
      group = "qbittorrent";
    }
  ];

  vpnNamespaces."airvpn" = {
    portMappings = [
      {
        from = 8080;
        to = 8080;
        protocol = "tcp";
      }
    ];

    openVPNPorts = [
      {
        port = 49948;
        protocol = "both";
      }
    ];
  };

  users.groups.qbittorrent = {};
  users.users.qbittorrent.group = "qbittorrent";
  users.users.qbittorrent.home = "/var/lib/qbittorrent";
  users.users.qbittorrent.isSystemUser = true;

  systemd.services.qbittorrent = {
    vpnConfinement = {
      enable = true;
      vpnNamespace = "airvpn";
    };

    wantedBy = ["multi-user.target"];
    after = ["network-online.target"];
    requires = ["airvpn.service"];

    environment = {
      QBT_PROFILE = "/var/lib/qbittorrent";
      QBT_WEBUI_PORT = "8080";
    };

    serviceConfig = {
      Type = "simple";
      User = "qbittorrent";
      Group = "qbittorrent";

      ExecStart = "${pkgs.qbittorrent-nox}/bin/qbittorrent-nox";
    };
  };
}
