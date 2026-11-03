{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mod head filter;
  inherit (builtins) div toJSON;
in
{
  config = {

    services.grafana.settings.dashboards.default_home_dashboard_path = "${
      (head (
        filter (
          provider: provider.name == "hosts"
        ) config.services.grafana.provision.dashboards.settings.providers
      )).options.path
    }/hosts.json";

    services.grafana.provision.dashboards.settings.providers = [
      {
        name = "hosts";
        options.path = pkgs.writeTextDir "hosts.json" (
          toJSON (
            let
              panel = id: title: expr: unit: {
                inherit id title;
                type = "timeseries";
                datasource = {
                  type = "prometheus";
                  uid = "\${datasource}";
                };
                gridPos = {
                  x = if mod id 2 == 1 then 0 else 12;
                  y = 8 * div (id - 1) 2;
                  w = 12;
                  h = 8;
                };
                targets = [
                  {
                    inherit expr;
                    # dedup btrfs subvol/bind mount spam, max usage wins per host+device
                    legendFormat =
                      if id == 4 || id == 8 then "{{host}} {{device}}" else "{{host}} {{instance}} {{name}} {{url}}";
                    refId = "A";
                  }
                ];
                fieldConfig.defaults.unit = unit;
              };
            in
            {
              uid = "hosts-overview";
              title = "Hosts and forwarding";
              schemaVersion = 39;
              version = 1;
              refresh = "30s";
              time = {
                from = "now-6h";
                to = "now";
              };
              templating.list = [
                {
                  name = "datasource";
                  label = "Storage";
                  type = "datasource";
                  query = "prometheus";
                  current = {
                    text = "VPS Prometheus";
                    value = "vps-prometheus";
                  };
                }
              ];
              panels = [
                (panel 1 "Scrape health (0 = unreachable)" "up" "short")
                (panel 2 "CPU busy"
                  "100 * (1 - avg by (host, instance) (rate(node_cpu_seconds_total{mode=\"idle\"}[5m])))"
                  "percent"
                )
                (panel 3 "Memory used" "100 * (1 - node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)"
                  "percent"
                )
                (panel 4 "Filesystem used"
                  "topk by (host, device) (1, 100 * (1 - node_filesystem_avail_bytes{fstype!~\"tmpfs|ramfs|squashfs|overlay\"} / node_filesystem_size_bytes))"
                  "percent"
                )
                (panel 5 "Failed systemd units"
                  "sum by (host, instance) (node_systemd_unit_state{state=\"failed\"})"
                  "short"
                )
                (panel 6 "Pending forwarding data" "vmagent_remotewrite_pending_data_bytes" "bytes")
                (panel 7 "Dropped forwarding data blocks / second"
                  "rate(vmagent_remotewrite_packets_dropped_total[5m])"
                  "ops"
                )
                (panel 8 "Network receive / second" "rate(node_network_receive_bytes_total{device!=\"lo\"}[5m])"
                  "Bps"
                )
              ];
            }
          )
        );
      }
    ];
  };
}
