{ lib, ... }:
{
  config = {
    services.nginx.statusPage = true;

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

    systemd.services.postgresql.postStart = lib.mkAfter ''
      psql -v ON_ERROR_STOP=1 -d postgres -c 'GRANT pg_monitor TO postgres_exporter'
    '';

    systemd.services.prometheus-postgres-exporter = {
      after = [ "postgresql.service" ];
      requires = [ "postgresql.service" ];
    };

    services.gatus.settings = {
      metrics = true;
      web.address = "127.0.0.1";
    };
    services.nginx.virtualHosts."meowcloud.net".locations."= /metrics".extraConfig = "return 404;";

    services.prosody.settings = {
      modules_enabled = [ "http_openmetrics" ];
      openmetrics_allow_ips = [
        "127.0.0.1"
        "::1"
      ];
    };
  };
}
