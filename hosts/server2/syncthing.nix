{ config, ... }:
let
  inherit (config.meow.dns) names;
in
{
  config = {
    meow.impermanence.directories = [
      {
        path = config.services.syncthing.dataDir;
        user = config.services.syncthing.user;
        group = config.services.syncthing.group;
        permissions = "0700";
      }
    ];

    services.nginx.virtualHosts.${names.syncthing} = {
      root = "/fake";
      forceSSL = true;
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:8384";
          recommendedProxySettings = false;
          extraConfig = ''
            proxy_set_header Host $proxy_host;
          '';
        };
      };
    };

    users.users.${config.services.syncthing.user}.extraGroups = [ "media" ];
    systemd.services.syncthing.serviceConfig.UMask = "0002";

    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      guiAddress = "127.0.0.1:8384";

      overrideDevices = false;
      overrideFolders = false;

      settings.gui = {
        insecureAdminAccess = false;
        insecureSkipHostCheck = false;
      };
    };
  };
}
