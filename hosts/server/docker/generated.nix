# Auto-generated using compose2nix v0.3.1.
{ pkgs, lib, ... }:

{
  # Runtime
  virtualisation.docker = {
    enable = true;
    autoPrune.enable = true;
  };
  virtualisation.oci-containers.backend = "docker";

  # Containers
  virtualisation.oci-containers.containers."bazarr" = {
    image = "lscr.io/linuxserver/bazarr:latest";
    environment = {
      "PGID" = "1000";
      "PUID" = "1000";
      "TZ" = "Europe/Helsinki";
    };
    volumes = [
      "/nix/persist/media:/media:rw"
      "/nix/persist/media/downloads/torrents:/downloads:rw"
      "/nix/persist/torrent_stack/config/bazarr:/config:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-bazarr" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."firefox" = {
    image = "lscr.io/linuxserver/firefox:latest";
    environment = {
      "PASSWORD" = "";
    };
    volumes = [
      "/nix/persist/torrent_stack/config/firefox:/config:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-firefox" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."gluetun" = {
    image = "qmcgaw/gluetun";
    environment = {
      "FIREWALL_VPN_INPUT_PORTS" = "49948";
      "SERVER_COUNTRIES" = "Sweden";
      "TZ" = "Europe/Helsinki";
      "VPN_SERVICE_PROVIDER" = "airvpn";
      "VPN_TYPE" = "wireguard";
    };
    environmentFiles = [
      "/run/torrent_stack.env"
    ];
    ports = [
      "127.0.0.1:8080:8080/tcp"
      "127.0.0.1:5030:5030/tcp"
      "127.0.0.1:7878:7878/tcp"
      "127.0.0.1:8989:8989/tcp"
      "127.0.0.1:8686:8686/tcp"
      "127.0.0.1:6767:6767/tcp"
      "127.0.0.1:9696:9696/tcp"
      "127.0.0.1:3000:3000/tcp"
    ];
    log-driver = "journald";
    extraOptions = [
      "--cap-add=NET_ADMIN"
      "--device=/dev/net/tun:/dev/net/tun:rwm"
      "--network-alias=gluetun"
      "--network=uwu_default"
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
      "docker-network-uwu_default.service"
    ];
    requires = [
      "docker-network-uwu_default.service"
    ];
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."lidarr" = {
    image = "lscr.io/linuxserver/lidarr:latest";
    environment = {
      "PGID" = "1000";
      "PUID" = "1000";
      "TZ" = "Europe/Helsinki";
    };
    volumes = [
      "/nix/persist/media:/media:rw"
      "/nix/persist/media/downloads/torrents:/downloads:rw"
      "/nix/persist/torrent_stack/config/lidarr:/config:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-lidarr" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."prowlarr" = {
    image = "lscr.io/linuxserver/prowlarr:latest";
    environment = {
      "PGID" = "1000";
      "PUID" = "1000";
      "TZ" = "Europe/Helsinki";
    };
    volumes = [
      "/nix/persist/media:/media:rw"
      "/nix/persist/media/downloads/torrents:/downloads:rw"
      "/nix/persist/torrent_stack/config/prowlarr:/config:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-prowlarr" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."qbittorrent" = {
    image = "lscr.io/linuxserver/qbittorrent:latest";
    environment = {
      "PGID" = "1000";
      "PUID" = "1000";
      "TORRENTING_PORT" = "49948";
      "TZ" = "Europe/Helsinki";
      "WEBUI_PORT" = "8080";
    };
    volumes = [
      "/nix/persist/media/downloads/torrents:/downloads:rw"
      "/nix/persist/torrent_stack/config/qbittorrent:/config:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-qbittorrent" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."radarr" = {
    image = "lscr.io/linuxserver/radarr:latest";
    environment = {
      "PGID" = "1000";
      "PUID" = "1000";
      "TZ" = "Europe/Helsinki";
    };
    volumes = [
      "/nix/persist/media:/media:rw"
      "/nix/persist/media/downloads/torrents:/downloads:rw"
      "/nix/persist/torrent_stack/config/radarr:/config:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-radarr" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."slskd" = {
    image = "slskd/slskd";
    environment = {
      "GID" = "1000";
      "SLSKD_REMOTE_CONFIGURATION" = "true";
      "UID" = "1000";
    };
    volumes = [
      "/nix/persist/media:/media:rw"
      "/nix/persist/media/downloads/soulseek:/app/downloads:rw"
      "/nix/persist/media/downloads/soulseek/incomplete:/app/incomplete:rw"
      "/nix/persist/torrent_stack/config/slskd:/app:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-slskd" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."sonarr" = {
    image = "lscr.io/linuxserver/sonarr:latest";
    environment = {
      "PGID" = "1000";
      "PUID" = "1000";
      "TZ" = "Europe/Helsinki";
    };
    volumes = [
      "/nix/persist/media:/media:rw"
      "/nix/persist/media/downloads/torrents:/downloads:rw"
      "/nix/persist/torrent_stack/config/sonarr:/config:rw"
    ];
    dependsOn = [
      "gluetun"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-sonarr" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."soularr" = {
    image = "mrusse08/soularr:latest";
    environment = {
      "SCRIPT_INTERVAL" = "1; exit";
      "TZ" = "Europe/Helsinki";
    };
    volumes = [
      "/nix/persist/media/downloads/soulseek:/nix/persist/media/downloads/soulseek:rw"
      "/nix/persist/torrent_stack/config/soularr:/data:rw"
    ];
    labels = {
      "compose2nix.settings.autoStart" = "false";
    };
    dependsOn = [
      "gluetun"
    ];
    user = "1000:1000";
    log-driver = "journald";
    autoStart = false;
    extraOptions = [
      "--network=container:gluetun"
    ];
  };
  systemd.services."docker-soularr" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "no";
    };
  };
  virtualisation.oci-containers.containers."watchtower" = {
    image = "containrrr/watchtower:latest";
    environment = {
      "TZ" = "Europe/Helsinki";
    };
    volumes = [
      "/var/run/docker.sock:/var/run/docker.sock:rw"
    ];
    ports = [
      "8081:8080/tcp"
    ];
    cmd = [ "--interval" "480" "--no-restart" "--http-api-metrics" "--http-api-token" "token" ];
    log-driver = "journald";
    extraOptions = [
      "--network-alias=watchtower"
      "--network=uwu_default"
    ];
  };
  systemd.services."docker-watchtower" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    after = [
      "docker-network-uwu_default.service"
    ];
    requires = [
      "docker-network-uwu_default.service"
    ];
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };

  # Networks
  systemd.services."docker-network-uwu_default" = {
    path = [ pkgs.docker ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStop = "docker network rm -f uwu_default";
    };
    script = ''
      docker network inspect uwu_default || docker network create uwu_default
    '';
    partOf = [ "docker-compose-uwu-root.target" ];
    wantedBy = [ "docker-compose-uwu-root.target" ];
  };

  # Root service
  # When started, this will automatically create all resources and start
  # the containers. When stopped, this will teardown all resources.
  systemd.targets."docker-compose-uwu-root" = {
    unitConfig = {
      Description = "Root target generated by compose2nix.";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
