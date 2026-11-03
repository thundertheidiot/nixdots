{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    getExe
    mapAttrs'
    mkMerge
    nameValuePair
    ;
  namespace = config.vpnNamespaces.airvpn;
  vpn = import "${inputs.self.outPath}/sops/wireguard";
  address = vpn.peers.home2.address;
  arrPorts = {
    radarr = 9708;
    sonarr = 9709;
    prowlarr = 9710;
  };
in
{
  config = mkMerge [
    {
      meow.wireguard.tcpPorts = [
        9708
        9709
        9710
        9711
        9712
        9713
      ];
      services.prometheus.exporters = mapAttrs' (
        name: port:
        nameValuePair "exportarr-${name}" {
          enable = true;
          listenAddress = address;
          inherit port;
          openFirewall = false;
          url = "http://${namespace.namespaceAddress}:${
            toString config.nixflix.${name}.config.hostConfig.port
          }";
          apiKeyFile = "/run/media-stack/secrets/${name}-api-key";
          environment.INTERFACE = address;
        }
      ) arrPorts;
      systemd.services = mapAttrs' (
        name: _:
        nameValuePair "prometheus-exportarr-${name}-exporter" {
          after = [
            "airvpn.service"
            "${name}.service"
            "wg-quick-wg0.service"
          ];
          wants = [
            "airvpn.service"
            "wg-quick-wg0.service"
          ];
          serviceConfig.RestartSec = 10;
        }
      ) arrPorts;
      sops.secrets = mapAttrs' (
        name: _:
        nameValuePair "${name}-api-key" {
          restartUnits = [ "prometheus-exportarr-${name}-exporter.service" ];
        }
      ) arrPorts;
    }
    {
      services.prometheus.exporters.exportarr-bazarr = {
        enable = true;
        listenAddress = address;
        port = 9711;
        openFirewall = false;
        url = "http://${namespace.namespaceAddress}:${toString config.services.bazarr.settings.general.port}";
        environment = {
          INTERFACE = address;
        };
      };
      systemd.services.prometheus-exportarr-bazarr-exporter = {
        environment.API_KEY_FILE = "/run/prometheus-exportarr-bazarr/api-key";
        after = [
          "airvpn.service"
          "bazarr.service"
          "wg-quick-wg0.service"
        ];
        wants = [
          "airvpn.service"
          "wg-quick-wg0.service"
        ];
        serviceConfig = {
          LoadCredential = [ "bazarr-config:${config.services.bazarr.dataDir}/config/config.yaml" ];
          RuntimeDirectory = "prometheus-exportarr-bazarr";
          RestartSec = 10;
        };
        preStart =
          let
            python = pkgs.python3.withPackages (p: [ p.pyyaml ]);
            extract = pkgs.writeText "bazarr-metrics-key.py" ''
              import os
              from pathlib import Path
              import yaml

              source = Path(os.environ["CREDENTIALS_DIRECTORY"]) / "bazarr-config"
              key = yaml.safe_load(source.read_text())["auth"]["apikey"]
              if not isinstance(key, str) or not key.strip():
                  raise ValueError("bazarr api key is missing")
              Path("/run/prometheus-exportarr-bazarr/api-key").write_text(key.strip())
            '';
          in
          "${getExe python} ${extract}";
      };
    }
    {
      sops.secrets.radarr-qbit-username.restartUnits = [ "prometheus-qbittorrent-exporter.service" ];
      sops.secrets.radarr-qbit-password.restartUnits = [ "prometheus-qbittorrent-exporter.service" ];
      systemd.services.prometheus-qbittorrent-exporter = {
        description = "qBittorrent metrics";
        wantedBy = [ "multi-user.target" ];
        after = [
          "airvpn.service"
          "qbittorrent.service"
          "wg-quick-wg0.service"
        ];
        wants = [
          "airvpn.service"
          "wg-quick-wg0.service"
        ];
        environment = {
          QBITTORRENT_BASE_URL = "http://${namespace.namespaceAddress}:8080";
          QBITTORRENT_COOKIE_NAME = "QBT_SID_8080";
          EXPORTER_HOST = address;
          EXPORTER_PORT = "9712";
          ENABLE_TRACKER = "false";
          ENABLE_HIGH_CARDINALITY = "false";
          ENABLE_INCREASED_CARDINALITY = "false";
        };
        script = ''
          export QBITTORRENT_USERNAME="$(< "$CREDENTIALS_DIRECTORY/username")"
          export QBITTORRENT_PASSWORD_FILE="$CREDENTIALS_DIRECTORY/password"
          exec ${getExe pkgs.prometheus-qbittorrent-exporter} -e
        '';
        serviceConfig = {
          DynamicUser = true;
          LoadCredential = [
            "username:/run/media-stack/secrets/radarr-qbit-username"
            "password:/run/media-stack/secrets/radarr-qbit-password"
          ];
          Restart = "on-failure";
          RestartSec = 10;
          UMask = "0077";
          NoNewPrivileges = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          PrivateTmp = true;
          PrivateDevices = true;
          RestrictAddressFamilies = [
            "AF_INET"
            "AF_INET6"
          ];
          CapabilityBoundingSet = "";
        };
      };
    }
    {
      services.nginx.virtualHosts =
        mapAttrs'
          (
            name: endpoint:
            nameValuePair "${name}-metrics" {
              listen = [
                {
                  addr = address;
                  port = endpoint.port;
                  ssl = false;
                }
              ];
              serverName = "_";
              locations = {
                "= /metrics".proxyPass =
                  "http://${namespace.namespaceAddress}:${toString endpoint.upstream}/metrics";
                "/".return = "404";
              };
            }
          )
          {
            slskd = {
              port = 9713;
              upstream = 5030;
            };
          }
        // {
          "soulseek.home".locations."~* ^/metrics".return = "404";
        };
      # the yaml credential overrides service settings, but has no metrics section
      systemd.services.slskd.environment = {
        SLSKD_METRICS = "true";
        SLSKD_METRICS_NO_AUTH = "true";
      };
    }
  ];
}
