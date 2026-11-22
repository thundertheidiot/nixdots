{ config, ... }:
let
  inherit (config.meow.dns) names;
in
{
  config = {
    services.navidrome.settings.Prometheus.Enabled = true;
    services.nginx.virtualHosts.${names.navidrome}.locations."= /metrics".return = "404";
  };
}
