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
