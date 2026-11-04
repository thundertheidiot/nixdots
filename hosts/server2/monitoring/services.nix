{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  vpn = import "${inputs.self.outPath}/sops/wireguard";
  address = vpn.peers.home2.address;
  exporterRole = "prometheus-postgres-exporter";
  psql = lib.getExe' config.services.postgresql.package "psql";
  grantPgMonitor = pkgs.writeShellScript "grant-postgres-exporter-monitor" ''
    exec ${psql} -v ON_ERROR_STOP=1 --dbname=postgres <<'SQL'
    SELECT 'CREATE USER "${exporterRole}"' WHERE NOT EXISTS (SELECT FROM pg_roles WHERE rolname = '${exporterRole}') \gexec
    GRANT pg_monitor TO "${exporterRole}";
    SQL
  '';
in
{
  services.comin.exporter.listen_address = "";

  services.forgejo.settings.metrics.ENABLED = true;
  services.nginx.virtualHosts."git.home".locations."= /metrics" = {
    return = "404";
  };
  services.nginx.virtualHosts."forgejo-metrics" = {
    listen = [
      {
        addr = address;
        port = 9114;
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
    port = 9187;
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
    port = 9113;
    scrapeUri = "http://127.0.0.1/nginx_status";
    openFirewall = false;
  };
  systemd.services.prometheus-nginx-exporter = {
    wants = [ "wg-quick-wg0.service" ];
    after = [ "wg-quick-wg0.service" ];
  };
}
