{
  config,
  infra,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe;
  inherit (infra.monitoring) storage;
  dash = config.meow.dns.names.grafana;
in
{
  config = {

    services.grafana = {
      enable = true;
      settings = {
        server = {
          http_addr = "127.0.0.1";
          http_port = 3000;
          domain = dash;
          root_url = "https://${dash}/";
        };
        security = {
          secret_key = "$__file{${config.services.grafana.dataDir}/secret-key}";
          cookie_secure = true;
        };
        users.allow_sign_up = false;
        auth.disable_login_form = true;
        "auth.basic".enabled = false;
        "auth.anonymous" = {
          enabled = true;
          org_role = "Viewer";
        };
        analytics = {
          reporting_enabled = false;
          check_for_updates = false;
        };
      };
      provision = {
        enable = true;

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
            url = "http://${infra.hosts.${storage.host}.wg}:${toString storage.port}";
          }
        ];
      };
    };

    systemd.services.grafana.preStart = ''
      umask 077
      if [ ! -s "${config.services.grafana.dataDir}/secret-key" ]; then
        ${getExe pkgs.openssl} rand -base64 48 > "${config.services.grafana.dataDir}/secret-key"
      fi
    '';

    services.nginx.virtualHosts.${dash} = {
      onlySSL = true;
      listen = [
        {
          addr = infra.hosts.vps.wg;
          port = 443;
          ssl = true;
        }
      ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:3000";
        proxyWebsockets = true;
      };
    };
  };
}
