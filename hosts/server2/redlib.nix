{ config, ... }:
let
  nya = config.meow.dns.domain;
in
{
  config = {
    services.nginx.virtualHosts."reddit.${nya}" = {
      forceSSL = true;
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
