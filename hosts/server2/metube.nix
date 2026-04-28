{config, ...}: let
  certs = import ../../certs;
in {
  server.domains = [
    "metube.home"
  ];

  services.nginx.virtualHosts."metube.home" = {
    forceSSL = true;
    sslCertificate = certs."local.crt";
    sslCertificateKey = config.sops.secrets.localKey.path;
    locations = {
      "/" = {
        proxyPass = "http://127.0.0.1:8081";
        recommendedProxySettings = true;
        extraConfig = ''
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection "Upgrade";
        '';
      };
    };
  };

  virtualisation.oci-containers.containers.metube = {
    image = "ghcr.io/alexta69/metube:latest";
    ports = ["8081:8081"];

    volumes = [
      "/mnt/storage/media/youtube/videos:/downloads"
      "/mnt/storage/media/my_music/youtube:/audio_downloads"
    ];

    environment = {
      DOWNLOAD_DIR = "/downloads";
      AUDIO_DOWNLOAD_DIR = "/audio_downloads";

      OUTPUT_TEMPLATE = "%(uploader)s/%(title)s.%(ext)s";
      OUTPUT_TEMPLATE_CHAPTER = "%(uploader)/%(title)s - %(section_number)s %(section_title)s.%(ext)s";

      YTDL_OPTIONS = builtins.toJSON {
        writethumbnail = false;
        embedthumbnail = true;
        addmetadata = true;
        postprocessors = [
          {
            key = "FFmpegExtractAudio";
            preferredcodec = "mp3";
            preferredquality = "0"; # VBR best quality
          }
          {
            key = "FFmpegSplitChapters";
            force_keyframes = false;
          }
          {
            key = "EmbedThumbnail";
          }
          {
            key = "FFmpegMetadata";
            add_metadata = true;
          }
        ];
      };

      UID = "1000";
      GID = "1000";
    };
  };
}
