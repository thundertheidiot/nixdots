{ server, ... }: {
  config = {
    services.vmagent = {
      enable = true;
      # keep url order stable, it identifies the on-disk queues
      # do not set remoteWrite.url, its module default uses disposable cache
      extraArgs = [
        "-httpListenAddr=127.0.0.1:8429"
        "-remoteWrite.url=http://127.0.0.1:9090/api/v1/write"
        "-remoteWrite.url=http://${server.homeServer2}:8428/api/v1/write"
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
                targets = [ "${server.homeServer2}:9100" ];
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
                targets = [ "${server.homeServer2}:9113" ];
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
                targets = [ "${server.homeServer2}:9187" ];
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
                targets = [ "${server.homeServer2}:9153" ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "media";
            static_configs =
              map
                (target: {
                  targets = [ "${server.homeServer2}:${toString target.port}" ];
                  labels = {
                    host = "server2";
                    service = target.name;
                  };
                })
                [
                  {
                    name = "radarr";
                    port = 9708;
                  }
                  {
                    name = "sonarr";
                    port = 9709;
                  }
                  {
                    name = "prowlarr";
                    port = 9710;
                  }
                  {
                    name = "bazarr";
                    port = 9711;
                  }
                  {
                    name = "qbittorrent";
                    port = 9712;
                  }
                  {
                    name = "slskd";
                    port = 9713;
                  }
                  ];
          }
          {
            job_name = "jellyfin";
            static_configs = [
              {
                targets = [ "${server.homeServer2}:8096" ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "navidrome";
            static_configs = [
              {
                targets = [ "${server.homeServer2}:4533" ];
                labels.host = "server2";
              }
            ];
          }
          {
            job_name = "forgejo";
            static_configs = [
              {
                targets = [ "${server.homeServer2}:9114" ];
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
