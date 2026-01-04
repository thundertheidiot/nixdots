{
  config,
  lib,
  inputs,
  pkgs,
  ...
}: {
  config = {
    server.domains = [
      "homeassistant.home"
    ];

    meow.impermanence.directories = [
      {
        path = "/var/lib/hass";
        user = "hass";
      }
    ];

    sops.secrets.home_assistant_secrets = {
      restartUnits = ["home-assistant.service"];
      key = "";
      path = "/var/lib/hass/secrets.yaml";
      owner = "hass";
    };

    services.home-assistant = {
      enable = true;
      extraComponents = [
        "analytics"
        "met"
        "isal"
        "mqtt"
      ];

      customComponents = [
        (pkgs.callPackage ./ha-bambulab.nix {})
      ];

      config = {
        default_config = {};

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
