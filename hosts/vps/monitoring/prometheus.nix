{ ... }: {
  config = {
    services.prometheus = {
      enable = true;
      listenAddress = "127.0.0.1";
      retentionTime = "15d";
      # vmagent is the only scraper, prometheus receives its local replica
      scrapeConfigs = [ ];
      extraFlags = [
        "--web.enable-remote-write-receiver"
        "--storage.tsdb.retention.size=512MB"
      ];
      exporters.node = {
        enable = true;
        listenAddress = "127.0.0.1";
        enabledCollectors = [ "systemd" ];
      };
    };
  };
}
