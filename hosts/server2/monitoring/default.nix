{ config, infra, ... }:
let
  address = infra.hosts.server2.wg;
  inherit (config.meow.server) monitoring;
in
{
  imports = [
    ./services.nix
    ./media-apps.nix
    ./media-extra.nix
    ./jellyfin.nix
  ];

  config = {
    meow.server.monitoring = {
      exporters.node.port = 9100;
      storage = 8428;
    };

    services.prometheus.exporters.node = {
      enable = true;
      listenAddress = address;
      port = monitoring.exporters.node.port;
      enabledCollectors = [ "systemd" ];
      openFirewall = false;
    };

    services.victoriametrics = {
      enable = true;
      listenAddress = "${address}:${toString monitoring.storage}";
      retentionPeriod = "1y";
    };

    meow.impermanence.directories = [
      {
        path = "/var/lib/private/${config.services.victoriametrics.stateDir}";
        permissions = "0700";
      }
    ];

    systemd.services.prometheus-node-exporter = {
      wants = [ "wg-quick-wg0.service" ];
      after = [ "wg-quick-wg0.service" ];
    };
    systemd.services.victoriametrics = {
      wants = [ "wg-quick-wg0.service" ];
      after = [ "wg-quick-wg0.service" ];
      unitConfig.RequiresMountsFor = [ "/var/lib/private/${config.services.victoriametrics.stateDir}" ];
    };
  };
}
