{ ... }:
{
  config = {
    services.navidrome.settings.Prometheus.Enabled = true;
    services.nginx.virtualHosts."navidrome.home".locations."= /metrics".return = "404";
  };
}
