{ config, ... }:
{
  imports = [
    (import ../../../certs).module
    ./services.nix
    ./prometheus.nix
    ./vmagent.nix
    ./grafana.nix
    ./dashboard.nix
  ];

  config = {
    meow.impermanence.directories = [
      {
        path = "/var/lib/${config.services.prometheus.stateDir}";
        user = "prometheus";
        group = "prometheus";
        permissions = "0700";
      }
      {
        path = "/var/lib/private/vmagent";
        permissions = "0700";
      }
      {
        path = config.services.grafana.dataDir;
        user = "grafana";
        group = "grafana";
        permissions = "0700";
      }
    ];
  };
}
