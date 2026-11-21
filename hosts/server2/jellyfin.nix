{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  inherit (lib)
    concatMap
    mkForce
    mkMerge
    mkOverride
    ;
  nya = config.meow.dns.domain;
  dataDir = "/var/lib/jellyfin";
in
{
  config = mkMerge [
    {
      # paths, ids and settings of the pre-nixflix install
      nixflix.jellyfin = {
        enable = true;
        apiKey._secret = config.sops.secrets.jellyfin_api_key.path;
        group = "jellyfin";
        inherit dataDir;
        configDir = "${dataDir}/config";
        cacheDir = "${dataDir}/cache";
        logDir = "${dataDir}/log";

        system = {
          serverName = "server";
          cacheSize = 400;
          metadataPath = "${dataDir}/metadata";
          enableLegacyAuthorization = false;
          pluginRepositories."Jellyfin Stable" = {
            url = "https://repo.jellyfin.org/files/plugin/manifest.json";
            # only fetched for repo-resolved plugins
            hash = "sha256-wdG7DDfZFwjudxF27n9hUQQLqsKEkT/C1B7qg0qmxVU=";
          };
        };

        encoding = {
          hardwareAccelerationType = "none";
          hardwareDecodingCodecs = [
            "h264"
            "vc1"
          ];
        };

        # imperative libraries for now
        libraries = {
          Movies = mkForce null;
          Shows = mkForce null;
        };

        # nixflix only creates the user if it's missing
        users.Thunder = {
          policy.isAdministrator = true;
          password._secret = config.sops.secrets.jellyfin_admin_password.path;
        };
      };

      # legacy permissions
      users.users.jellyfin.uid = mkOverride 10 997;
      users.groups.jellyfin.gid = mkOverride 10 997;

      networking.firewall.allowedUDPPorts = [
        1900
        7359
      ];

      meow.impermanence.directories = [
        {
          path = dataDir;
          persistPath = "${config.meow.impermanence.persist}/jellyfin";
          user = "jellyfin";
          group = "jellyfin";
          permissions = "0750";
        }
      ];

      # nixflix opens the data dir up to 0755
      systemd.tmpfiles.settings."10-jellyfin".${dataDir}.d.mode = mkForce "0750";
    }
    {
      meow.sops.enableSecrets = [ "kanidm_jellyfin" ];

      services.kanidm.provision.systems.oauth2.jellyfin = {
        displayName = "Jellyfin";
        originUrl = concatMap (domain: [
          "https://${domain}/sso/OID/redirect/kanidm"
          "https://${domain}/sso/OID/r/kanidm"
        ]) ([ "jellyfin.${nya}" ] ++ import ../vps/jellyfin-domains.nix);
        originLanding = "https://jellyfin.${nya}/";
        basicSecretFile = config.sops.secrets.kanidm_jellyfin.path;
        preferShortUsername = true;
        scopeMaps.admins = [
          "openid"
          "email"
          "profile"
          "groups"
        ];
      };

      # 9p4's plugin is archived and stops at jellyfin 10.11, this fork targets 12
      nixflix.jellyfin.plugins."SSO Authentication" = {
        enable = true;
        package = inputs.nixflix.lib.buildJellyfinPlugin { inherit pkgs; } rec {
          pname = "sso-authentication";
          version = "5.1.1";
          src = pkgs.fetchurl {
            url = "https://github.com/K0lin/jellyfin-plugin-sso/releases/download/v${version}/sso-authentication_${version}.zip";
            hash = "sha256-2xJrR2O9o6N1T6IQMjL8A+8b+SnP7aNfLprznIdSM7E=";
          };
        };
        apiName = "SSO-Auth";
        config.OidConfigs.kanidm = {
          Enabled = true;
          OidEndpoint = "https://home.idm.meowcloud.net/oauth2/openid/jellyfin";
          OidClientId = "jellyfin";
          OidSecret._secret = config.sops.secrets.kanidm_jellyfin.path;
          DefaultUsernameClaim = "preferred_username";
          SchemeOverride = "https";
          # kanidm decides who gets in, jellyfin permissions stay as they are
          EnableAuthorization = false;
          RoleClaim = "groups";
          OidScopes = [ ];
          AdminRoles = [ ];
          Roles = [ ];
          EnabledFolders = [ ];
          FolderRoleMapping = [ ];
          LiveTvRoles = [ ];
          LiveTvManagementRoles = [ ];
        };
      };

      nixflix.jellyfin.branding.loginDisclaimer = ''
        <form action="/sso/OID/start/kanidm">
          <button class="raised block emby-button button-submit">Sign in with Kanidm</button>
        </form>
      '';
    }
    {
      services.nginx.virtualHosts."jellyfin.${nya}" = {
        forceSSL = true;

        # recommendedTlsSettings = false;
        locations = {
          "/" = {
            proxyPass = "http://127.0.0.1:8096";
            recommendedProxySettings = false;
            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_set_header X-Forwarded-Protocol $scheme;
              proxy_set_header X-Forwarded-Host $http_host;

              # Disable buffering when the nginx proxy gets very resource heavy upon streaming
              proxy_buffering off;
            '';
          };

          "/socket" = {
            proxyPass = "http://127.0.0.1:8096";
            recommendedProxySettings = false;
            extraConfig = ''
              proxy_http_version 1.1;
              proxy_set_header Upgrade $http_upgrade;
              proxy_set_header Connection "upgrade";
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
              proxy_set_header X-Forwarded-Protocol $scheme;
              proxy_set_header X-Forwarded-Host $http_host;
            '';
          };
        };

        extraConfig = ''
                  # listen 80;
                  # listen [::]:80;

                  client_max_body_size 20M;
                  add_header X-Frame-Options "SAMEORIGIN";
                  add_header X-XSS-Protection "0"; # Do NOT enable. This is obsolete/dangerous ????
                  add_header X-Content-Type-Options "nosniff";

                  add_header Permissions-Policy "accelerometer=(), ambient-light-sensor=(), battery=(), bluetooth=(), camera=(), clipboard-read=(), display-capture=(), document-domain=(), encrypted-media=(), gamepad=(), geolocation=(), gyroscope=(), hid=(), idle-detection=(), interest-cohort=(), keyboard-map=(), local-fonts=(), magnetometer=(), microphone=(), payment=(), publickey-credentials-get=(), serial=(), sync-xhr=(), usb=(), xr-spatial-tracking=()" always;

                  # add_header Content-Security-Policy "default-src https: data: blob: ; img-src 'self' https://* ; style-src 'self' 'unsafe-inline'; script-src 'self' 'unsafe-inline' https://www.gstatic.com https://www.youtube.com blob:; worker-src 'self' blob:; connect-src 'self'; object-src 'none'; frame-ancestors 'self'";

          # add_header Content-Security-Policy "default-src https: data: blob: http://image.tmdb.org; style-src 'self' 'unsafe-inline' https://cdn.jsdelivr.net/npm/jellyskin@latest/dist/main.css; script-src 'self' 'unsafe-inline' https://www.gstatic.com/cv/js/sender/v1/cast_sender.js https://www.youtube.com blob:; worker-src 'self' blob:; connect-src 'self'; object-src 'none'; frame-ancestors 'self'";

          add_header Content-Security-Policy "default-src * 'unsafe-inline' 'unsafe-eval'; script-src * 'unsafe-inline' 'unsafe-eval'; connect-src * 'unsafe-inline'; img-src * data: blob: 'unsafe-inline'; frame-src *; style-src * 'unsafe-inline';";
        '';
      };
    }
  ];
}
