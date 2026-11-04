{ config, lib, ... }:
let
  inherit (lib) mkAfter;
  inherit (builtins) toFile;
in
{
  config = {
    services.comin.exporter.listen_address = "127.0.0.1";

    services.prometheus.exporters.dnsmasq = {
      enable = true;
      listenAddress = "127.0.0.1";
      dnsmasqListenAddress = "10.100.0.1:53";
      leasesPath = toFile "dnsmasq-empty-leases" "";
    };

    services.coturn.extraConfig = mkAfter ''
      prometheus
      prometheus-address=127.0.0.1
      prometheus-port=9641
    '';

    services.nginx.statusPage = true;
    services.nginx.virtualHosts."navidrome.meowcloud.net".locations."= /metrics".return = "404";

    services.prometheus.exporters.nginx = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9113;
      scrapeUri = "http://127.0.0.1/nginx_status";
    };

    services.prometheus.exporters.postgres = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9187;
      user = "postgres_exporter";
      group = "postgres_exporter";
      dataSourceName = "user=postgres_exporter dbname=postgres host=/run/postgresql sslmode=disable";
    };

    users.groups.postgres_exporter = { };
    users.users.postgres_exporter = {
      isSystemUser = true;
      group = "postgres_exporter";
    };

    services.postgresql = {
      ensureUsers = [
        {
          name = "postgres_exporter";
          ensureClauses.login = true;
        }
      ];
      authentication = lib.mkOverride 10 ''
        local all postgres_exporter peer
      '';
    };

    systemd.services.postgres-exporter-grant = {
      after = [ "postgresql-setup.service" ];
      requires = [ "postgresql-setup.service" ];
      serviceConfig = {
        Type = "oneshot";
        User = "postgres";
      };
      script = ''
        ${lib.getExe' config.services.postgresql.package "psql"} -v ON_ERROR_STOP=1 -d postgres <<'SQL'
        SELECT 'CREATE USER postgres_exporter' WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'postgres_exporter') \gexec
        GRANT pg_monitor TO postgres_exporter;
        SQL
      '';
    };

    systemd.services.prometheus-postgres-exporter = {
      after = [
        "postgresql.service"
        "postgres-exporter-grant.service"
      ];
      requires = [
        "postgresql.service"
        "postgres-exporter-grant.service"
      ];
    };

    services.gatus.settings = {
      metrics = true;
      web.address = "127.0.0.1";
    };

    services.prosody.settings = {
      modules_enabled = [
        "http_openmetrics"
        "statistics"
      ];
      statistics_interval = "60";
      openmetrics_allow_ips = [
        "127.0.0.1"
        "::1"
      ];
    };
  };
}
