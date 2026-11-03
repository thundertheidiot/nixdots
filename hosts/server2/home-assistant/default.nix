{
  pkgs,
  ...
}:
{
  config = {
    meow.impermanence.directories = [
      {
        path = "/var/lib/hass";
        user = "hass";
        group = "hass";
        permissions = "0700";
      }
    ];

    sops.secrets.home_assistant_secrets = {
      restartUnits = [ "home-assistant.service" ];
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
        (pkgs.callPackage ./ha-bambulab.nix { })
      ];

      config = {
        default_config = { };

        homeassistant = {
          unit_system = "metric";
          time_zone = "Europe/Helsinki";
          name = "Home";
        };

        http = {
          use_x_forwarded_for = true;
          trusted_proxies = "127.0.0.1";
          server_port = 8123;
        };
      };
    };

  };
}
