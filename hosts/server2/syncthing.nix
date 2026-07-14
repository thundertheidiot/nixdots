{config, ...}: let
  certs = import ../../certs;
in {
  config = {
    meow.impermanence.directories = [
      config.services.syncthing.dataDir
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
          recommendedProxySettings = true;
        };
      };
    };

    services.syncthing = {
      enable = true;
      openDefaultPorts = true;

      overrideDevices = false;
      overrideFolders = false;

      settings.gui = {
        insecureAdminAccess = true;
        insecureSkipHostCheck = true;
      };
    };
  };
}
