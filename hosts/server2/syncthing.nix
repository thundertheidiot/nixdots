{ config, ... }:
let
  nya = config.meow.dns.domain;
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

    services.nginx.virtualHosts."syncthing.${nya}" = {
      root = "/fake";
      forceSSL = true;
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:8384";
          recommendedProxySettings = false;
          extraConfig = ''
            # Keep the admin GUI available only on the home network until authentication is provisioned.
            allow 127.0.0.1;
            allow ::1;
            allow 192.168.101.0/24;
            deny all;
            proxy_set_header Host $proxy_host;
          '';
        };
      };
    };

    services.syncthing = {
      enable = true;
      openDefaultPorts = true;
      # The CLI bind takes precedence over a potentially unsafe persisted GUI address.
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
