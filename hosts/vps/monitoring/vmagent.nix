{
  config,
  infra,
  lib,
  ...
}:
let
  inherit (lib) groupBy mapAttrsToList;
  inherit (config.meow.server) cluster;
  hostName = config.networking.hostName;

  # local exporters listen on loopback, the rest on their wg address
  at =
    host: port: "${if host == hostName then "127.0.0.1" else infra.hosts.${host}.wg}:${toString port}";
  storage = at cluster.storage.host cluster.storage.port;
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
        "-remoteWrite.url=http://${storage}/api/v1/write"
        "-remoteWrite.tmpDataPath=/var/lib/vmagent/remote-write"
        # 500 MiB per destination (1000 MiB total) + metadata/headroom
        "-remoteWrite.maxDiskUsagePerURL=524288000"
        # preserve sample order for the prometheus receiver
        "-remoteWrite.queues=1"
        "-remoteWrite.forcePromProto"
      ];
      prometheusConfig = {
        global.scrape_interval = "30s";
        scrape_configs =
          mapAttrsToList (job: exporters: {
            job_name = job;
            static_configs = map (exporter: {
              targets = [ (at exporter.host exporter.port) ];
              labels = {
                inherit (exporter) host;
              }
              // exporter.labels;
            }) exporters;
          }) (groupBy (exporter: exporter.job) cluster.exporters)
          ++ [
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
