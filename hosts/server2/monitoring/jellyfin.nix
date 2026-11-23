{ config, ... }:
{
  config = {
    nixflix.jellyfin.system.enableMetrics = true;
    meow.server.monitoring.exporters.jellyfin.port = config.meow.server.services.jellyfin.port;
  };
}
