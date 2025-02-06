{...}: let
  inherit (builtins) mapAttrs;
in {
  imports = [
    ./generated.nix
  ];

  meow.sops.enableSecrets = ["server_torrent_stack_env"];
  meow.sops.secrets."server_torrent_stack_env" = {
    path = "/run/torrent_stack.env";
    mode = "0644";
  };

  server.domains = [
    "firefox.local"
    "torrent.local"
    "soulseek.local"
    "radarr.local"
    "sonarr.local"
    "lidarr.local"
    "bazarr.local"
    "prowlarr.local"
    "immich.local"
    "homeassistant.local"
  ];

  services.nginx.virtualHosts =
    mapAttrs (_: port: {
      root = "/fake";
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
      "torrent.local" = 8080;
      "radarr.local" = 7878;
      "sonarr.local" = 8989;
      "lidarr.local" = 8686;
      "bazarr.local" = 6767;
      "prowlarr.local" = 9696;
      "homeassistant.local" = 8123;
    }
    // {
      "immich.local" = {
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

      "soulseek.local" = {
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

      "firefox.local" = {
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
