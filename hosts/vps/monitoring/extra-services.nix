{ config, ... }:
{
  services.matrix-continuwuity.settings.global.allow_prometheus_metrics = true;

  services.nginx.virtualHosts.${config.meow.server.matrix.domain}.locations."= /_continuwuity/metrics".extraConfig = ''
    return 404;
  '';
}
