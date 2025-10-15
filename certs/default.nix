{
  module = {config, ...}: {
    sops.secrets.rootCA = {
      sopsFile = ./rootCA.key;
      format = "binary";
    };

    sops.secrets.localKey = {
      sopsFile = ./local.key;
      format = "binary";
      owner = config.services.nginx.user;
    };

    security.pki.certificateFiles = [./rootCA.pem];
  };

  domains = [
    "auth.home"
    "n8n.home"
    "git.home"
    "jellyfin.home"
    "reddit.home"
    "firefox.home"
    "torrent.home"
    "radarr.home"
    "sonarr.home"
    "prowlarr.home"
    "lidarr.home"
    "bazarr.home"
    "immich.home"
    "homeassistant.home"
  ];

  "local.crt" = ./local.crt;
  "local.csr" = ./local.csr;
  "rootCA.pem" = ./rootCA.pem;
  "san.ext" = ./san.ext;
}
