{
  config,
  pkgs,
  ...
}:
let
  certs = import ../../certs;
  soulbeet = pkgs.callPackage ../../pkgs/soulbeet.nix { };
in
{
  server.domains = [
    "navidrome.home"
    "soulbeet.home"
  ];

  services.nginx.virtualHosts."navidrome.home" = {
    forceSSL = true;
    sslCertificate = certs."local.crt";
    sslCertificateKey = config.sops.secrets.localKey.path;
    locations = {
      "/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.navidrome.settings.Port}";
        recommendedProxySettings = true;
        extraConfig = ''
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "Upgrade";
        '';
      };
    };
  };

  sops.secrets.navidrome_env = {
    owner = config.services.navidrome.user;
  };

  systemd.services.navidrome.serviceConfig.BindReadOnlyPaths = [ "/mnt/storage/media" ];
  systemd.services.navidrome.environment.TMPDIR = "${config.meow.impermanence.persist}/navidrome/tmp";
  systemd.tmpfiles.rules = [
    "d ${config.meow.impermanence.persist}/navidrome/tmp 0700 navidrome navidrome -"
  ];

  services.navidrome = {
    enable = true;
    openFirewall = true;
    environmentFile = config.sops.secrets.navidrome_env.path;
    settings = {
      Address = "0.0.0.0";
      MusicFolder = "/mnt/storage/media/music";
      DataFolder = "${config.meow.impermanence.persist}/navidrome";
      BaseUrl = "https://navidrome.meowcloud.net";

      Agents = "deezer,lastfm,listenbrainz";

      LastFM.Enabled = true;
      Deezer.Enabled = true;
      ListenBrainz.Enabled = true;
    };
  };

  services.nginx.virtualHosts."soulbeet.home" = {
    forceSSL = true;
    sslCertificate = certs."local.crt";
    sslCertificateKey = config.sops.secrets.localKey.path;
    locations = {
      "/" = {
        proxyPass = "http://127.0.0.1:9765";
        recommendedProxySettings = true;
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "Upgrade";
          proxy_read_timeout 3600s;
          proxy_send_timeout 3600s;
        '';
      };
    };
  };

  systemd.services.soulbeet = {
    description = "Soulbeet";
    wantedBy = [ "multi-user.target" ];
    after = [
      "network-online.target"
    ];
    wants = [ "network-online.target" ];
    unitConfig.RequiresMountsFor = [
      "/nix/persist/media-stack/soulbeet"
      "/mnt/storage"
    ];
    path = [ pkgs.beets ];
    environment = {
      DATABASE_URL = "sqlite:/data/soulbeet.db";
      DOWNLOAD_PATH = "/downloads";
      NAVIDROME_URL = "http://127.0.0.1:${toString config.services.navidrome.settings.Port}";
      BEETS_ALBUM_MODE = "true";
      BEETS_CONFIG = "${soulbeet}/beets_config.yaml";
      HOME = "/tmp";
      IP = "127.0.0.1";
      PORT = "9765";
    };
    serviceConfig = {
      User = "root";
      Group = "root";
      EnvironmentFile = config.sops.secrets.soulbeet_env.path;
      WorkingDirectory = "${soulbeet}";
      ExecStart = "${soulbeet}/server";
      Restart = "on-failure";
      BindPaths = [
        "/nix/persist/media-stack/soulbeet:/data"
        "/mnt/storage/media/my_music:/music"
        "/mnt/storage/media/downloads/soulseek:/downloads"
      ];
    };
  };
}
