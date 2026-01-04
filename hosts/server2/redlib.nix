{config, ...}: let
  certs = import ../../certs;
in {
  config = {
    server.domains = [
      "reddit.local"
      "reddit.home"
    ];

    services.nginx.virtualHosts."reddit.local" = {
      serverAliases = ["reddit.home"];
      forceSSL = true;
      sslCertificate = certs."local.crt";
      sslCertificateKey = config.sops.secrets.localKey.path;
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.redlib.port}";
          recommendedProxySettings = true;
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
          '';
        };
      };
    };

    services.redlib = {
      enable = true;
      address = "127.0.0.1";
      port = 8083;
      settings = {
        # ENABLE_RSS = "on";

        THEME = "dark";
        POST_SORT = "top";
        SHOW_NSFW = "on";
        BLUR_NSFW = "on";
        DISABLE_VISIT_REDDIT_CONFIRMATION = "on";

        USE_HLS = "on";
      };
    };
  };
}
