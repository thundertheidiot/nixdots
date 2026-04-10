{config, ...}: let
  certs = import ../../certs;
in {
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

  systemd.services.navidrome.serviceConfig.BindReadOnlyPaths = ["/mnt/storage/media"];
  systemd.services.navidrome.environment.TMPDIR = "${config.meow.impermanence.persist}/navidrome/tmp";
  systemd.tmpfiles.rules = ["d ${config.meow.impermanence.persist}/navidrome/tmp 0700 navidrome navidrome -"];

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

  virtualisation.oci-containers.containers.soulbeet = {
    image = "docker.io/docccccc/soulbeet:latest";
    ports = [
      "9765:9765"
    ];
    environment = {
      "DATABASE_URL" = "sqlite:/data/soulbeet.db";
      "DOWNLOAD_PATH" = "/downloads";
      "NAVIDROME_URL" = "http://host.docker.internal:${toString config.services.navidrome.settings.Port}";
      "BEETS_ALBUM_MODE" = "true";
    };
    environmentFiles = [config.sops.secrets.soulbeet_env.path];
    volumes = [
      "/mnt/storage/media:/mnt/storage/media:rw"
      "/mnt/storage/media/my_music:/music:rw"
      "/nix/persist/soulbeet:/data"
      "/mnt/storage/media/downloads/soulseek:/downloads"
    ];

    extraOptions = ["--add-host=host.docker.internal:host-gateway" "--network=server2_default"];
  };
}
