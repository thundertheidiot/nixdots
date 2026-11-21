{ config, ... }:
let
  nya = config.meow.dns.domain;
in
{
  config = {
    services.navidrome.settings.Prometheus.Enabled = true;
    services.nginx.virtualHosts."navidrome.${nya}".locations."= /metrics".return = "404";
  };
}
