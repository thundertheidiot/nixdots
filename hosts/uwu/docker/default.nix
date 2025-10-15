{
  config,
  inputs,
  ...
}: let
  inherit (builtins) mapAttrs;
  certs = import "${inputs.self.outPath}/certs";
in {
  imports = [
    ./generated.nix
  ];

  sops.secrets."torrent_stack_env" = {
    path = "/run/torrent_stack.env";
    mode = "0644";
  };

  server.domains = [
    "firefox.home"
    "torrent.home"
    "soulseek.home"
    "radarr.home"
    "sonarr.home"
    "lidarr.home"
    "bazarr.home"
    "prowlarr.home"
    "immich.home"
    "homeassistant.home"
  ];

  services.nginx.virtualHosts =
    mapAttrs (_: port: {
      root = "/fake";

      sslCertificate = certs."local.crt";
      sslCertificateKey = config.sops.secrets.localKey.path;

      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:${toString port}";
          recommendedProxySettings = true;
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
          '';
        };
      };
    }) {
      "torrent.home" = 8080;
      "radarr.home" = 7878;
      "sonarr.home" = 8989;
      "lidarr.home" = 8686;
      "bazarr.home" = 6767;
      "prowlarr.home" = 9696;
      "homeassistant.home" = 8123;
    }
    // {
      "immich.home" = {
        sslCertificate = certs."local.crt";
        sslCertificateKey = config.sops.secrets.localKey.path;

        locations."/" = {
          proxyPass = "http://127.0.0.1:2283";
          recommendedProxySettings = true;
          extraConfig = ''
            client_max_body_size 50000M;

            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";
            proxy_redirect off;

            proxy_read_timeout 600s;
            proxy_send_timeout 600s;
            send_timeout 600s;
          '';
        };
      };

      "soulseek.home" = {
        sslCertificate = certs."local.crt";
        sslCertificateKey = config.sops.secrets.localKey.path;

        locations."/" = {
          proxyPass = "http://127.0.0.1:5030";
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
            proxy_set_header Host $host;
            proxy_request_buffering off;
            client_max_body_size 0;
          '';
        };
      };

      "firefox.home" = {
        sslCertificate = certs."local.crt";
        sslCertificateKey = config.sops.secrets.localKey.path;

        root = "/fake";
        locations."/" = {
          proxyPass = "http://127.0.0.1:3000/";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Host $proxy_host;
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "upgrade";

            proxy_set_header        X-Real-IP $remote_addr;
            proxy_set_header        X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header        X-Forwarded-Proto $scheme;

            proxy_read_timeout      1800s;
            proxy_send_timeout      1800s;
            proxy_connect_timeout   1800s;
            proxy_buffering         off;

            client_max_body_size 10M;
          '';
        };
      };
    };
}
