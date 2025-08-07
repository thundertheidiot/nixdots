{
  config,
  lib,
  inputs,
  ...
}: {
  config = {
    server.domains = [
      "homeassistant.home"
    ];

    meow.impermanence.directories = [
      {
        path = "/var/lib/hass";
      }
    ];

    services.home-assistant = {
      enable = true;
      config = {
        homeassistant = {
          unit_system = "metric";
          time_zone = "Europe/Helsinki";
          name = "Home";
        };

        http = {
          use_x_forwarded_for = true;
          trusted_proxies = "127.0.0.1";
        };
      };
    };

    services.nginx.virtualHosts."homeassistant.home" = {
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.home-assistant.config.http.server_port}";
          recommendedProxySettings = true;
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
          '';
        };
      };
    };
  };
}
