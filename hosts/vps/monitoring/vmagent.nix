{ infra, ... }:
let
  inherit (infra.monitoring) exporters storage;
  server2 = infra.hosts.server2.wg;
  ports = exporters.server2;
  at = port: "${server2}:${toString port}";
  media = [
    "radarr"
    "sonarr"
    "prowlarr"
    "bazarr"
    "qbittorrent"
    "slskd"
  ];
in
{
  config = {
    services.vmagent = {
      enable = true;
      # keep url order stable, it identifies the on-disk queues
      # do not set remoteWrite.url, its module default uses disposable cache
      extraArgs = [
        "-httpListenAddr=127.0.0.1:8429"
        "-remoteWrite.url=http://127.0.0.1:9090/api/v1/write"
        "-remoteWrite.url=http://${infra.hosts.${storage.host}.wg}:${toString storage.port}/api/v1/write"
        "-remoteWrite.tmpDataPath=/var/lib/vmagent/remote-write"
        # 500 MiB per destination (1000 MiB total) + metadata/headroom
        "-remoteWrite.maxDiskUsagePerURL=524288000"
        # preserve sample order for the prometheus receiver
        "-remoteWrite.queues=1"
        "-remoteWrite.forcePromProto"
      ];
      prometheusConfig = {
        global.scrape_interval = "30s";
        scrape_configs = [
          {
            job_name = "node";
            static_configs = [
              {
                targets = [ "127.0.0.1:9100" ];
                labels.host = "vps";
              }
              {
                targets = [ (at ports.node) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "comin";
            static_configs = [
              {
                targets = [ "127.0.0.1:4243" ];
                labels.host = "vps";
              }
              {
                targets = [ (at ports.comin) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "nginx";
            static_configs = [
              {
                targets = [ "127.0.0.1:9113" ];
                labels.host = "vps";
              }
              {
                targets = [ (at ports.nginx) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "postgres";
            static_configs = [
              {
                targets = [ "127.0.0.1:9187" ];
                labels.host = "vps";
              }
              {
                targets = [ (at ports.postgres) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "dnsmasq";
            static_configs = [
              {
                targets = [ "127.0.0.1:9153" ];
                labels.host = "vps";
              }
              {
                targets = [ (at ports.dnsmasq) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "media";
            static_configs = map (service: {
              targets = [ (at ports.${service}) ];
              labels = {
                host = "server2";
                inherit service;
              };
            }) media;
          }
          {
            job_name = "jellyfin";
            static_configs = [
              {
                targets = [ (at ports.jellyfin) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "navidrome";
            static_configs = [
              {
                targets = [ (at ports.navidrome) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "forgejo";
            static_configs = [
              {
                targets = [ (at ports.forgejo) ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "coturn";
            static_configs = [
              {
                targets = [ "127.0.0.1:9641" ];
                labels.host = "vps";
              }
            ];
          }
          {
            job_name = "gatus";
            static_configs = [
              {
                targets = [ "127.0.0.1:8081" ];
                labels.host = "vps";
              }
            ];
          }
          {
            job_name = "prosody";
            static_configs = [
              {
                targets = [ "127.0.0.1:5280" ];
                labels.host = "vps";
              }
            ];
          }
          {
            job_name = "vmagent";
            static_configs = [ { targets = [ "127.0.0.1:8429" ]; } ];
          }
        ];
      };
    };
    systemd.services.vmagent.serviceConfig = {
      StateDirectory = "vmagent";
      StateDirectoryMode = "0700";
    };
  };
}
