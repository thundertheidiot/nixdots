{ config, ... }:
{
  config = {
    services.navidrome.settings.Prometheus.Enabled = true;
    meow.server.monitoring.exporters.navidrome.port = config.meow.server.services.navidrome.port;
  };
}
