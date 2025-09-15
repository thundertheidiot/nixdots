{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf str;

  inherit (lib) mkIf mkMerge head length listToAttrs;

  cfg = config.meow.server;
in {
  # https://github.com/NixOS/nixpkgs/pull/260006
  disabledModules = ["services/networking/prosody.nix"];
  imports = [./prosody.nix];

  options.meow.server.xmppDomains = mkOpt (listOf str) [] {};

  config = mkIf (length cfg.xmppDomains > 0) (let
    mainDomain = head cfg.xmppDomains;
    subd = d: s: "${s}.${d}";
    msubd = subd mainDomain;
  in {
    meow.server.certificates = cfg.xmppDomains;

    security.acme.certs."${mainDomain}" = {
      extraDomainNames = map msubd ["chat" "share" "proxy"];
    };

    meow.impermanence.directories = [
      {path = config.services.prosody.dataDir;}
    ];

    users.users."${config.services.prosody.user}".extraGroups = ["acme" "turnserver"];

    networking.firewall.allowedTCPPorts = config.services.prosody.settings.legacy_ssl_ports;

    services.nginx.virtualHosts = {
      "${mainDomain}" = {
        locations."/http-bind" = {
          proxyPass = "http://127.0.0.1:5280/http-bind";
          recommendedProxySettings = false;
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Connection "Upgrade";
            proxy_set_header Upgrade $http_upgrade;

            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };

        locations."/xmpp-websocket" = {
          proxyPass = "http://127.0.0.1:5280/xmpp-websocket";
          recommendedProxySettings = false;
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Connection "Upgrade";
            proxy_set_header Upgrade $http_upgrade;

            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
            proxy_read_timeout 900s;
          '';
        };

        locations."/.well-known/host-meta" = {
          proxyPass = "http://127.0.0.1:5280/.well-known/host-meta";
          extraConfig = ''
            default_type 'application/xrd+xml';
            add_header Access-Control-Allow-Origin '*' always;
          '';
        };

        locations."/.well-known/host-meta.json" = {
          proxyPass = "http://127.0.0.1:5280/.well-known/host-meta.json";
          extraConfig = ''
            default_type 'application/jrd+json';
            add_header Access-Control-Allow-Origin '*' always;
          '';
        };

        locations."/file_share" = {
          proxyPass = "http://127.0.0.1:5280/file_share";
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_set_header Connection "Upgrade";
            proxy_set_header Upgrade $http_upgrade;

            proxy_set_header Host $host;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      };
    };

    services.prosody = {
      enable = true;
      openFirewall = true;
      # requires http_upload instead of http_file_share
      xmppComplianceSuite = false;

      package = pkgs.prosody.override {
        withCommunityModules = ["http_altconnect"];
      };

      virtualHosts = listToAttrs (map (name: {
          inherit name;
          value = {
            useACMEHost = name;
          };
        })
        cfg.xmppDomains);

      components = {
        "share.${mainDomain}" = {
          module = "http_file_share";
          settings = {
            http_upload_file_size_limit = "100*1024*1024";
            http_upload_file_daily_quota = "1024*1024*1024";
            http_upload_file_global_quota = "1024*1024*2048";

            http_host = "${mainDomain}";

            http_external_url = "https://${mainDomain}";

            ssl = {
              certificate = "/var/lib/acme/${mainDomain}/fullchain.pem";
              # prosody searches for privkey.pem
              key = "/var/lib/acme/${mainDomain}/key.pem";
            };
          };
        };

        "chat.${mainDomain}" = {
          module = "muc";
          settings = {
            restrict_room_creation = "local";
            muc_room_default_public = false;
            muc_room_default_members_only = true;

            ssl = {
              certificate = "/var/lib/acme/${mainDomain}/fullchain.pem";
              # prosody searches for privkey.pem
              key = "/var/lib/acme/${mainDomain}/key.pem";
            };
          };
        };
      };

      settings = mkMerge [
        (mkIf cfg.coturn {
          turn_external_host = mainDomain;
          turn_external_port = config.services.coturn.listening-port;
          modules_enabled = ["turn_external" "external_services"];

          external_services = [
            {
              type = "stun";
              transport = "udp";
              port = config.services.coturn.listening-port;
              host = cfg.mainDomain;
              secret = true;
            }
            {
              type = "turn";
              transport = "udp";
              port = config.services.coturn.listening-port;
              host = cfg.mainDomain;
              secret = true;
            }
          ];
        })
        {
          prosodyctl_service_warnings = false;

          modules_enabled = [
            "admin_shell"
            "csi_simple"
            "bosh"
            "websocket"
          ];

          cross_domain_bosh = true;
          cross_domain_websocket = true;

          trusted_proxies = ["127.0.0.1" "::1"];

          disco_items = [
            ["chat.${mainDomain}" "multi user chat"]
            ["share.${mainDomain}" "file upload"]
            ["proxy.${mainDomain}" "proxy"]
          ];

          default_storage = "sql";
          sql = {
            driver = "SQLite3";
            database = "prosody.sqlite";
          };

          legacy_ssl_ports = [5223];

          ssl = {
            certificate = "/var/lib/acme/${mainDomain}/fullchain.pem";
            # prosody searches for privkey.pem
            key = "/var/lib/acme/${mainDomain}/key.pem";
          };

          # if this is not set it tries to search a nonexistent directory and prosodyctl crashes
          certificates = "/var/lib/acme";
        }
      ];

      extraConfig = mkIf cfg.coturn ''
        local function read_file(path)
          local file = io.open(path, "r")
          if not file then
            return nil, "Could not open file: " .. path
          end
          local content = file:read("*a")
          file:close()
          return content
        end

        turn_external_secret = read_file("${config.sops.secrets.coturn_secret.path}")
        external_service_secret = read_file("${config.sops.secrets.coturn_secret.path}")
      '';
    };
  });
}
