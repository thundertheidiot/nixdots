# This file is not loaded inside the container
{...}: let
  inherit (builtins) mapAttrs;
in {
  server.domains = [
    "soulseek.local"
    "torrent.local"
  ];

  containers.vpnContainer.forwardPorts = [
    {
      # Slskd
      containerPort = 5030;
      hostPort = 5030;
      protocol = "tcp";
    }
    {
      # Transmission
      containerPort = 9091;
      hostPort = 9091;
      protocol = "tcp";
    }
    {
      # Radarr
      containerPort = 7878;
      hostPort = 7878;
      protocol = "tcp";
    }
    {
      # Sonarr
      containerPort = 8989;
      hostPort = 8989;
      protocol = "tcp";
    }
    {
      # Prowlarr
      containerPort = 9696;
      hostPort = 9696;
      protocol = "tcp";
    }
  ];

  services.nginx.virtualHosts =
    mapAttrs (_: port: {
      root = "/fake";
      locations = {
        "/" = {
          proxyPass = "http://10.10.10.1:${toString port}";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Host $proxy_host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Host $http_host;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      };
    }) {
      "soulseek.local" = 5030;
      "torrent.local" = 9091;
      "radarr.local" = 7878;
      "sonarr.local" = 8989;
      "prowlarr.local" = 9696;
    };
}
