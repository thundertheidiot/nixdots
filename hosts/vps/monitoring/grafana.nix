{
  config,
  inputs,
  lib,
  pkgs,
  server,
  ...
}:
let
  inherit (lib) getExe;
  vpn = import "${inputs.self.outPath}/sops/wireguard";
  address = vpn.peers.${vpn.hub}.address;
  nya = config.meow.dns.domain;
in
{
  config = {

    services.grafana = {
      enable = true;
      settings = {
        server = {
          http_addr = "127.0.0.1";
          http_port = 3000;
          domain = "dash.${nya}";
          root_url = "https://dash.${nya}/";
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
            url = "http://${server.homeServer2}:8428";
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

    services.nginx.virtualHosts."dash.${nya}" = {
      onlySSL = true;
      listen = [
        {
          addr = address;
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
