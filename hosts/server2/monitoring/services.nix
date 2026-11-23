{
  config,
  infra,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe' mapAttrs;
  address = infra.hosts.server2.wg;
  ports = mapAttrs (_: exporter: exporter.port) config.meow.server.monitoring.exporters;
  exporterRole = "prometheus-postgres-exporter";
  psql = getExe' config.services.postgresql.package "psql";
  grantPgMonitor = pkgs.writeShellScript "grant-postgres-exporter-monitor" ''
    exec ${psql} -v ON_ERROR_STOP=1 --dbname=postgres <<'SQL'
    SELECT 'CREATE USER "${exporterRole}"' WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '${exporterRole}') \gexec
    GRANT pg_monitor TO "${exporterRole}";
    SQL
  '';
in
{
  config = {
    meow.server.monitoring.exporters = {
      comin.port = 4243;
      forgejo.port = 9114;
      postgres.port = 9187;
      dnsmasq.port = 9153;
      nginx.port = 9113;
    };

    services.comin.exporter = {
      listen_address = address;
      port = ports.comin;
    };
    systemd.services.comin = {
      wants = [ "wg-quick-wg0.service" ];
      after = [ "wg-quick-wg0.service" ];
    };

    services.forgejo.settings.metrics.ENABLED = true;
    services.nginx.virtualHosts."forgejo-metrics" = {
      listen = [
        {
          addr = address;
          port = ports.forgejo;
          ssl = false;
        }
      ];
      serverName = "_";
      locations = {
        "= /metrics" = {
          proxyPass = "http://127.0.0.1:${toString config.services.forgejo.settings.server.HTTP_PORT}/metrics";
        };
        "/" = {
          return = "404";
        };
      };
    };

    services.postgresql.ensureUsers = [ { name = exporterRole; } ];
    users.users.${exporterRole} = {
      isSystemUser = true;
      group = "prometheus-postgres-exporter";
      extraGroups = [ "postgres" ];
    };
    users.groups.prometheus-postgres-exporter = { };
    services.prometheus.exporters.postgres = {
      enable = true;
      listenAddress = address;
      port = ports.postgres;
      user = exporterRole;
      group = exporterRole;
      dataSourceName = "user=${exporterRole} dbname=postgres host=/run/postgresql sslmode=disable";
      openFirewall = false;
    };

    systemd.services.postgres-exporter-grant = {
      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql-setup.service" ];
      requires = [ "postgresql-setup.service" ];
      before = [ "prometheus-postgres-exporter.service" ];
      serviceConfig = {
        Type = "oneshot";
        User = "postgres";
        ExecStart = grantPgMonitor;
      };
    };
    systemd.services.prometheus-postgres-exporter = {
      wants = [ "wg-quick-wg0.service" ];
      after = [
        "wg-quick-wg0.service"
        "postgresql.service"
        "postgres-exporter-grant.service"
      ];
      requires = [
        "postgresql.service"
        "postgres-exporter-grant.service"
      ];
    };

    services.prometheus.exporters.dnsmasq = {
      enable = true;
      listenAddress = address;
      port = ports.dnsmasq;
      dnsmasqListenAddress = "127.0.0.1:53";
      leasesPath = builtins.toFile "dnsmasq-empty-leases" "";
    };
    systemd.services.prometheus-dnsmasq-exporter = {
      wants = [ "wg-quick-wg0.service" ];
      after = [ "wg-quick-wg0.service" ];
    };

    services.nginx.statusPage = true;
    services.prometheus.exporters.nginx = {
      enable = true;
      listenAddress = address;
      port = ports.nginx;
      scrapeUri = "http://127.0.0.1/nginx_status";
      openFirewall = false;
    };
    systemd.services.prometheus-nginx-exporter = {
      wants = [ "wg-quick-wg0.service" ];
      after = [ "wg-quick-wg0.service" ];
    };
  };
}
