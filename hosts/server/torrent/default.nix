{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (builtins) mapAttrs toString;
in {
  config = {
    meow.sops.enableSecrets = ["server_torrent_stack_env"];
    meow.sops.secrets."server_torrent_stack_env" = {
      path = "/run/torrent_stack_env";
      mode = "0644";
    };

    # Auto-generated using compose2nix v0.3.2-pre.
    # Runtime
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = true;
    };
    virtualisation.oci-containers.backend = "docker";

    server.domains = [
      "firefox.box"
      "torrent.box"
      "soulseek.box"
      "radarr.box"
      "sonarr.box"
      "prowlarr.box"
    ];

    services.nginx.virtualHosts =
      mapAttrs (_: port: {
        root = "/fake";
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:${toString port}";
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
        "torrent.box" = 8080;
        "soulseek.box" = 5030;
        "radarr.box" = 7878;
        "sonarr.box" = 8989;
        "prowlarr.box" = 9696;
        "firefox.box" = 3000;
      }
      // {
        "firefox.box" = {
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

    # Containers
    virtualisation.oci-containers.containers."gluetun" = {
      image = "ghcr.io/qdm12/gluetun:v3";
      environmentFiles = [
        "/run/torrent_stack_env"
      ];
      environment = {
        "SERVER_COUNTRIES" = "Sweden";
        "TZ" = "Europe/Helsinki";
        "VPN_SERVICE_PROVIDER" = "airvpn";
        "VPN_TYPE" = "wireguard";
      };
      ports = [
        "127.0.0.1:8080:8080/tcp"
        "127.0.0.1:5030:5030/tcp"
        "127.0.0.1:3000:3000/tcp"
        "127.0.0.1:7878:7878/tcp"
        "127.0.0.1:8989:8989/tcp"
        "127.0.0.1:9696:9696/tcp"
      ];
      log-driver = "journald";
      extraOptions = [
        "--cap-add=NET_ADMIN"
        "--device=/dev/net/tun:/dev/net/tun:rwm"
        "--network-alias=gluetun"
        "--network=torrent_default"
      ];
    };
    systemd.services."docker-gluetun" = {
      serviceConfig = {
        Restart = lib.mkOverride 90 "always";
        RestartMaxDelaySec = lib.mkOverride 90 "1m";
        RestartSec = lib.mkOverride 90 "100ms";
        RestartSteps = lib.mkOverride 90 9;
      };
      after = [
        "docker-network-torrent_default.service"
      ];
      requires = [
        "docker-network-torrent_default.service"
      ];
      partOf = [
        "docker-compose-torrent-root.target"
      ];
      wantedBy = [
        "docker-compose-torrent-root.target"
      ];
    };
    virtualisation.oci-containers.containers."torrent-prowlarr" = {
      image = "lscr.io/linuxserver/prowlarr:latest";
      environmentFiles = [
        "/run/torrent_stack_env"
      ];
      environment = {
        "TZ" = "Europe/Helsinki";
      };
      volumes = [
        "/persist/torrent_stack/config/prowlarr:/config:rw"
      ];
      dependsOn = [
        "gluetun"
      ];
      log-driver = "journald";
      extraOptions = [
        "--network=container:gluetun"
      ];
    };
    systemd.services."docker-torrent-prowlarr" = {
      serviceConfig = {
        Restart = lib.mkOverride 90 "always";
        RestartMaxDelaySec = lib.mkOverride 90 "1m";
        RestartSec = lib.mkOverride 90 "100ms";
        RestartSteps = lib.mkOverride 90 9;
      };
      partOf = [
        "docker-compose-torrent-root.target"
      ];
      wantedBy = [
        "docker-compose-torrent-root.target"
      ];
    };
    virtualisation.oci-containers.containers."torrent-qbittorrent" = {
      image = "lscr.io/linuxserver/qbittorrent:latest";
      environmentFiles = [
        "/run/torrent_stack_env"
      ];
      environment = {
        "TORRENTING_PORT" = "49948";
        "TZ" = "Europe/Helsinki";
        "WEBUI_PORT" = "8080";
      };
      volumes = [
        "/persist/torrent_stack/config/qbittorrent:/config:rw"
        "/persist/torrent_stack/downloads/torrent:/downloads:rw"
      ];
      dependsOn = [
        "gluetun"
      ];
      log-driver = "journald";
      extraOptions = [
        "--network=container:gluetun"
      ];
    };
    systemd.services."docker-torrent-qbittorrent" = {
      serviceConfig = {
        Restart = lib.mkOverride 90 "always";
        RestartMaxDelaySec = lib.mkOverride 90 "1m";
        RestartSec = lib.mkOverride 90 "100ms";
        RestartSteps = lib.mkOverride 90 9;
      };
      partOf = [
        "docker-compose-torrent-root.target"
      ];
      wantedBy = [
        "docker-compose-torrent-root.target"
      ];
    };
    virtualisation.oci-containers.containers."torrent-radarr" = {
      image = "lscr.io/linuxserver/radarr:latest";
      environmentFiles = [
        "/run/torrent_stack_env"
      ];
      environment = {
        "TZ" = "Europe/Helsinki";
      };
      volumes = [
        "/persist/media:/media:rw"
        "/persist/torrent_stack/config/radarr:/config:rw"
        "/persist/torrent_stack/downloads/torrent:/downloads:rw"
      ];
      dependsOn = [
        "gluetun"
      ];
      log-driver = "journald";
      extraOptions = [
        "--network=container:gluetun"
      ];
    };
    systemd.services."docker-torrent-radarr" = {
      serviceConfig = {
        Restart = lib.mkOverride 90 "always";
        RestartMaxDelaySec = lib.mkOverride 90 "1m";
        RestartSec = lib.mkOverride 90 "100ms";
        RestartSteps = lib.mkOverride 90 9;
      };
      partOf = [
        "docker-compose-torrent-root.target"
      ];
      wantedBy = [
        "docker-compose-torrent-root.target"
      ];
    };
    virtualisation.oci-containers.containers."torrent-slskd" = {
      image = "slskd/slskd";
      environmentFiles = [
        "/run/torrent_stack_env"
      ];
      environment = {
        "SLSKD_REMOTE_CONFIGURATION" = "true";
      };
      volumes = [
        "/persist/torrent_stack/config/slskd:/app:rw"
        "/persist/torrent_stack/downloads/soulseek/downloads:/app/downloads:rw"
        "/persist/torrent_stack/downloads/soulseek/incomplete:/app/incomplete:rw"
      ];
      dependsOn = [
        "gluetun"
      ];
      log-driver = "journald";
      extraOptions = [
        "--network=container:gluetun"
      ];
    };
    systemd.services."docker-torrent-slskd" = {
      serviceConfig = {
        Restart = lib.mkOverride 90 "always";
        RestartMaxDelaySec = lib.mkOverride 90 "1m";
        RestartSec = lib.mkOverride 90 "100ms";
        RestartSteps = lib.mkOverride 90 9;
      };
      partOf = [
        "docker-compose-torrent-root.target"
      ];
      wantedBy = [
        "docker-compose-torrent-root.target"
      ];
    };
    virtualisation.oci-containers.containers."torrent-sonarr" = {
      image = "lscr.io/linuxserver/sonarr:latest";
      environmentFiles = [
        "/run/torrent_stack_env"
      ];
      environment = {
        "TZ" = "Europe/Helsinki";
      };
      volumes = [
        "/persist/media:/media:rw"
        "/persist/torrent_stack/config/sonarr:/config:rw"
        "/persist/torrent_stack/downloads/torrent:/downloads:rw"
      ];
      dependsOn = [
        "gluetun"
      ];
      log-driver = "journald";
      extraOptions = [
        "--network=container:gluetun"
      ];
    };
    systemd.services."docker-torrent-sonarr" = {
      serviceConfig = {
        Restart = lib.mkOverride 90 "always";
        RestartMaxDelaySec = lib.mkOverride 90 "1m";
        RestartSec = lib.mkOverride 90 "100ms";
        RestartSteps = lib.mkOverride 90 9;
      };
      partOf = [
        "docker-compose-torrent-root.target"
      ];
      wantedBy = [
        "docker-compose-torrent-root.target"
      ];
    };
    virtualisation.oci-containers.containers."torrent-firefox" = {
      image = "lscr.io/linuxserver/firefox:latest";
      environment = {
        "PASSWORD" = "";
      };
      volumes = [
        "/persist/torrent_stack/config/firefox:/config:rw"
      ];
      dependsOn = [
        "gluetun"
      ];
      log-driver = "journald";
      extraOptions = [
        "--network=container:gluetun"
        "--device=/dev/dri:/dev/dri"
      ];
    };
    systemd.services."docker-torrent-firefox" = {
      serviceConfig = {
        Restart = lib.mkOverride 90 "always";
        RestartMaxDelaySec = lib.mkOverride 90 "1m";
        RestartSec = lib.mkOverride 90 "100ms";
        RestartSteps = lib.mkOverride 90 9;
      };
      partOf = [
        "docker-compose-torrent-root.target"
      ];
      wantedBy = [
        "docker-compose-torrent-root.target"
      ];
    };

    # Networks
    systemd.services."docker-network-torrent_default" = {
      path = [pkgs.docker];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStop = "docker network rm -f torrent_default";
      };
      script = ''
        docker network inspect torrent_default || docker network create torrent_default
      '';
      partOf = ["docker-compose-torrent-root.target"];
      wantedBy = ["docker-compose-torrent-root.target"];
    };

    # Root service
    # When started, this will automatically create all resources and start
    # the containers. When stopped, this will teardown all resources.
    systemd.targets."docker-compose-torrent-root" = {
      unitConfig = {
        Description = "Root target generated by compose2nix.";
      };
      wantedBy = ["multi-user.target"];
    };
  };
}
