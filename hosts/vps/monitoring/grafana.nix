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
in
{
  config = {

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
          secret_key = "$__file{${config.services.grafana.dataDir}/secret-key}";
          cookie_secure = true;
        };
        users.allow_sign_up = false;
        auth.disable_login_form = true;
        "auth.basic".enabled = false;
        "auth.anonymous" = {
          enabled = true;
          org_role = "Admin";
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
  };
}
