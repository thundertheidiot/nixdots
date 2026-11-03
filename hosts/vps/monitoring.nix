{
  config,
  inputs,
  lib,
  pkgs,
  server,
  ...
}:
let
  inherit (lib) getExe mkMerge;
  vpn = import "${inputs.self.outPath}/sops/wireguard";
  address = vpn.peers.${vpn.hub}.address;
in
{
  imports = [ (import ../../certs).module ];

  config = mkMerge [
    {
      services.prometheus = {
        enable = true;
        listenAddress = "127.0.0.1";
        retentionTime = "15d";
        scrapeConfigs = [ ];
        extraFlags = [
          "--web.enable-remote-write-receiver"
          "--storage.tsdb.retention.size=512MB"
        ];
        exporters.node = {
          enable = true;
          listenAddress = "127.0.0.1";
          enabledCollectors = [ "systemd" ];
        };
      };

      services.vmagent = {
        enable = true;
        # Keep URL order stable: it identifies the on-disk queues.
        # Do not set remoteWrite.url: its module default uses disposable cache.
        extraArgs = [
          "-httpListenAddr=127.0.0.1:8429"
          "-remoteWrite.url=http://127.0.0.1:9090/api/v1/write"
          "-remoteWrite.url=http://${server.homeServer2}:8428/api/v1/write"
          "-remoteWrite.tmpDataPath=/var/lib/vmagent/remote-write"
          # 500 MiB per destination (1000 MiB total), plus metadata/headroom.
          # Oldest queued data is dropped when full; no indefinite outage guarantee.
          "-remoteWrite.maxDiskUsagePerURL=524288000"
          # Preserve sample order for the Prometheus receiver.
          "-remoteWrite.queues=1"
          "-remoteWrite.forcePromProto"
        ];
        prometheusConfig = {
          global.scrape_interval = "30s";
          scrape_configs = [
            {
              job_name = "node";
              static_configs = [
                {
                  targets = [ "127.0.0.1:9100" ];
                  labels.host = "vps";
                }
                {
                  targets = [ "${server.homeServer2}:9100" ];
                  labels.host = "server2";
                }
              ];
            }
            {
              job_name = "vmagent";
              static_configs = [ { targets = [ "127.0.0.1:8429" ]; } ];
            }
          ];
        };
      };

      systemd.services.vmagent.serviceConfig = {
        StateDirectory = "vmagent";
        StateDirectoryMode = "0700";
      };

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
    }

    {
      services.grafana = {
        enable = true;
        settings = {
          server = {
            http_addr = "127.0.0.1";
            http_port = 3000;
            domain = "dash.vps.home";
            root_url = "https://dash.vps.home/";
          };
          security = {
            admin_password = "$__file{${config.services.grafana.dataDir}/admin-password}";
            secret_key = "$__file{${config.services.grafana.dataDir}/secret-key}";
            cookie_secure = true;
          };
          users.allow_sign_up = false;
          "auth.anonymous".enabled = false;
          analytics = {
            reporting_enabled = false;
            check_for_updates = false;
          };
        };
        provision = {
          enable = true;
          dashboards.settings.providers = [
            {
              name = "hosts";
              options.path = pkgs.writeTextDir "hosts.json" (
                builtins.toJSON (
                  let
                    panel = id: title: expr: unit: {
                      inherit id title;
                      type = "timeseries";
                      datasource = {
                        type = "prometheus";
                        uid = "\${datasource}";
                      };
                      gridPos = {
                        x = if lib.mod id 2 == 1 then 0 else 12;
                        y = 8 * builtins.div (id - 1) 2;
                        w = 12;
                        h = 8;
                      };
                      targets = [
                        {
                          inherit expr;
                          legendFormat = "{{host}} {{instance}} {{name}} {{url}}";
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
                        "100 * (1 - node_filesystem_avail_bytes{fstype!~\"tmpfs|ramfs|squashfs|overlay\"} / node_filesystem_size_bytes)"
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
          datasources.settings.datasources = [
            {
              name = "VPS Prometheus";
              uid = "vps-prometheus";
              type = "prometheus";
              access = "proxy";
              url = "http://127.0.0.1:9090";
              isDefault = true;
            }
            {
              name = "Server2 VictoriaMetrics";
              uid = "server2-victoriametrics";
              type = "prometheus";
              access = "proxy";
              url = "http://${server.homeServer2}:8428";
            }
          ];
        };
      };

      # Runs as grafana before the server reads its config, after persistent mounts.
      # Read the bootstrap password as root: cat /var/lib/grafana/admin-password
      systemd.services.grafana.preStart = ''
        umask 077
        for name in admin-password secret-key; do
          if [ ! -s "${config.services.grafana.dataDir}/$name" ]; then
            ${getExe pkgs.openssl} rand -base64 48 > "${config.services.grafana.dataDir}/$name"
          fi
        done
      '';

      services.dnsmasq.settings.host-record = [ "dash.vps.home,${address}" ];
      services.nginx.virtualHosts."dash.vps.home" = {
        onlySSL = true;
        listen = [
          {
            addr = address;
            port = 443;
            ssl = true;
          }
        ];
        sslCertificate = (import "${inputs.self.outPath}/certs")."local.crt";
        sslCertificateKey = config.sops.secrets.localKey.path;
        locations."/" = {
          proxyPass = "http://127.0.0.1:3000";
          proxyWebsockets = true;
        };
      };
    }
  ];
}
