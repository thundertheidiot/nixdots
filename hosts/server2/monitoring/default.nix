{ config, infra, ... }:
let
  address = infra.hosts.server2.wg;
  ports = infra.monitoring.exporters.server2;
in
{
  imports = [
    ./services.nix
    ./media-apps.nix
    ./media-extra.nix
    ./jellyfin.nix
  ];

  config = {
    services.prometheus.exporters.node = {
      enable = true;
      listenAddress = address;
      port = ports.node;
      enabledCollectors = [ "systemd" ];
      openFirewall = false;
    };

    services.victoriametrics = {
      enable = true;
      listenAddress = "${address}:${toString infra.monitoring.storage.port}";
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
