{
  config,
  lib,
  inputs,
  ...
}: {
  config = {
    server.domains = [
      "n8n.home"
    ];

    meow.impermanence.directories = [
      {
        path = "/var/lib/private/n8n";
      }
    ];

    services.nginx.virtualHosts."n8n.home" = let
      certs = import "${inputs.self.outPath}/certs";
    in {
      forceSSL = lib.mkForce true;

      sslCertificate = certs."local.crt";
      sslCertificateKey = config.sops.secrets.localKey.path;

      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:${toString config.services.n8n.environment.N8N_PORT}";
          recommendedProxySettings = true;
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
          '';
        };
      };
    };

    services.n8n = {
      enable = true;
      openFirewall = true;
      environment = {
        N8N_SECURE_COOKIE = false;
      };
    };
  };
}
