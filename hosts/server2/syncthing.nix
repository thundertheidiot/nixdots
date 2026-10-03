{ config, ... }:
let
  certs = import ../../certs;
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

    server.domains = [
      "syncthing.server"
    ];

    services.nginx.virtualHosts."syncthing.server" = {
      root = "/fake";
      forceSSL = true;
      sslCertificate = certs."local.crt";
      sslCertificateKey = config.sops.secrets.localKey.path;
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:8384";
          recommendedProxySettings = false;
          extraConfig = ''
            # Keep admin access local until declarative authentication is provisioned.
            allow 127.0.0.1;
            allow ::1;
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
