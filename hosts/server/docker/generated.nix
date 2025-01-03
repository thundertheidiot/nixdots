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
  virtualisation.oci-containers.containers."immich_machine_learning" = {
    image = "ghcr.io/immich-app/immich-machine-learning:release";
    volumes = [
      "uwu_model-cache:/cache:rw"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network-alias=immich-machine-learning"
      "--network=uwu_server"
    ];
  };
  systemd.services."docker-immich_machine_learning" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    after = [
      "docker-network-uwu_server.service"
      "docker-volume-uwu_model-cache.service"
    ];
    requires = [
      "docker-network-uwu_server.service"
      "docker-volume-uwu_model-cache.service"
    ];
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."immich_postgres" = {
    image = "docker.io/tensorchord/pgvecto-rs:pg14-v0.2.0@sha256:90724186f0a3517cf6914295b5ab410db9ce23190a2d9d0b9dd6463e3fa298f0";
    environment = {
      "POSTGRES_DB" = "immich";
      "POSTGRES_INITDB_ARGS" = "--data-checksums";
      "POSTGRES_PASSWORD" = "postgres";
      "POSTGRES_USER" = "postgres";
    };
    volumes = [
      "/nix/persist/immich/database:/var/lib/postgresql/data:rw"
    ];
    cmd = [ "postgres" "-c" "shared_preload_libraries=vectors.so" "-c" "search_path=\"$user\", public, vectors" "-c" "logging_collector=on" "-c" "max_wal_size=2GB" "-c" "shared_buffers=512MB" "-c" "wal_compression=on" ];
    log-driver = "journald";
    extraOptions = [
      "--health-cmd=pg_isready --dbname=\"\${POSTGRES_DB}\" --username=\"\${POSTGRES_USER}\" || exit 1; Chksum=\"$(psql --dbname=\"\${POSTGRES_DB}\" --username=\"\${POSTGRES_USER}\" --tuples-only --no-align --command='SELECT COALESCE(SUM(checksum_failures), 0) FROM pg_stat_database')\"; echo \"checksum failure count is $Chksum\"; [ \"$Chksum\" = '0' ] || exit 1"
      "--health-interval=5m0s"
      "--health-start-interval=30s"
      "--health-start-period=5m0s"
      "--network-alias=database"
      "--network=uwu_server"
    ];
  };
  systemd.services."docker-immich_postgres" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    after = [
      "docker-network-uwu_server.service"
    ];
    requires = [
      "docker-network-uwu_server.service"
    ];
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."immich_redis" = {
    image = "docker.io/redis:6.2-alpine@sha256:eaba718fecd1196d88533de7ba49bf903ad33664a92debb24660a922ecd9cac8";
    log-driver = "journald";
    extraOptions = [
      "--health-cmd=redis-cli ping || exit 1"
      "--network-alias=redis"
      "--network=uwu_server"
    ];
  };
  systemd.services."docker-immich_redis" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    after = [
      "docker-network-uwu_server.service"
    ];
    requires = [
      "docker-network-uwu_server.service"
    ];
    partOf = [
      "docker-compose-uwu-root.target"
    ];
    wantedBy = [
      "docker-compose-uwu-root.target"
    ];
  };
  virtualisation.oci-containers.containers."immich_server" = {
    image = "ghcr.io/immich-app/immich-server:release";
    environment = {
      "DB_DATABASE_NAME" = "immich";
      "DB_PASSWORD" = "postgres";
      "DB_USERNAME" = "postgres";
      "IMMICH_MACHINE_LEARNING_ENABLED" = "false";
    };
    volumes = [
      "/etc/localtime:/etc/localtime:ro"
      "/nix/persist/immich/images:/usr/src/app/upload:rw"
    ];
    ports = [
      "2283:2283/tcp"
    ];
    dependsOn = [
      "immich_postgres"
      "immich_redis"
    ];
    log-driver = "journald";
    extraOptions = [
      "--network-alias=immich-server"
      "--network=uwu_server"
    ];
  };
  systemd.services."docker-immich_server" = {
    serviceConfig = {
      Restart = lib.mkOverride 90 "always";
      RestartMaxDelaySec = lib.mkOverride 90 "1m";
      RestartSec = lib.mkOverride 90 "100ms";
      RestartSteps = lib.mkOverride 90 9;
    };
    after = [
      "docker-network-uwu_server.service"
    ];
    requires = [
      "docker-network-uwu_server.service"
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
  systemd.services."docker-network-uwu_server" = {
    path = [ pkgs.docker ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStop = "docker network rm -f uwu_server";
    };
    script = ''
      docker network inspect uwu_server || docker network create uwu_server --driver=bridge
    '';
    partOf = [ "docker-compose-uwu-root.target" ];
    wantedBy = [ "docker-compose-uwu-root.target" ];
  };

  # Volumes
  systemd.services."docker-volume-uwu_model-cache" = {
    path = [ pkgs.docker ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      docker volume inspect uwu_model-cache || docker volume create uwu_model-cache
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
