{
  config = {
    services.nginx.virtualHosts."jellyfin.local".locations."= /metrics".return = "404";

    nixflix.jellyfin.system.enableMetrics = true;
  };
}
