{ config, inputs, ... }:
let
  vpn = import "${inputs.self.outPath}/sops/wireguard";
  address = vpn.peers.home2.address;
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
      port = 9100;
      enabledCollectors = [ "systemd" ];
      openFirewall = false;
    };

    services.victoriametrics = {
      enable = true;
      listenAddress = "${address}:8428";
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
