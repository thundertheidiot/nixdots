{
  config,
  lib,
  pkgs,
  infra,
  ...
}:
let
  inherit (lib) getExe;
  inherit (pkgs.mpkgs) soulbeet;

  dataDir = "${config.meow.impermanence.persist}/media-stack/soulbeet";
  port = 9765;
  url = "https://${config.meow.dns.names.soulbeet}";
  # the local kanidm, so sso keeps working without the vps
  issuer = "https://${infra.kanidm.nodes.server2.hostname}/oauth2/openid/soulbeet";
in
{
  config = {
    meow.server.services.soulbeet = { };

    services.nginx.virtualHosts.${config.meow.dns.names.soulbeet}.locations."/" = {
      proxyPass = "http://127.0.0.1:${toString port}";
      recommendedProxySettings = true;
      proxyWebsockets = true;
      extraConfig = ''
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
      '';
    };

    # public client, soulbeet always does pkce
    services.kanidm.provision.systems.oauth2.soulbeet = {
      displayName = "Soulbeet";
      originUrl = "${url}/api/auth/oidc/callback";
      originLanding = "${url}/";
      public = true;
      preferShortUsername = true;
      # only admins get a token
      scopeMaps.admins = [
        "openid"
        "profile"
      ];
    };

    users.groups.soulbeet = { };
    users.users.soulbeet = {
      isSystemUser = true;
      group = "soulbeet";
      extraGroups = [ "media" ];
    };

    systemd.services.soulbeet =
      let
        beetsPython =
          let
            mv = config.multiverse.instance;
            beetsPkgs = mv.at (mv.revOf "beets" "2.11.0");
          in
          beetsPkgs.python3.withPackages (p: [ p.beets ]);
      in
      {
        description = "Soulbeet";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        unitConfig.RequiresMountsFor = [
          dataDir
          "/mnt/storage"
        ];
        path = [ beetsPython ];
        environment = {
          DATABASE_URL = "sqlite:/data/soulbeet.db";
          DOWNLOAD_PATH = "/downloads";
          NAVIDROME_URL = "http://127.0.0.1:${toString config.services.navidrome.settings.Port}";
          BEETS_ALBUM_MODE = "true";
          BEETS_PYTHON = getExe beetsPython;
          BEETS_CONFIG = "${soulbeet}/beets_config.yaml";
          HOME = "/tmp";
          IP = "127.0.0.1";
          PORT = toString port;
          PUBLIC_URL = url;
          OIDC_ISSUER_URL = issuer;
          OIDC_CLIENT_ID = "soulbeet";
          OIDC_PROVIDER_NAME = "Kanidm";
          OIDC_SCOPES = "openid profile";
          # no accounts from sso, the admin links theirs in settings
          OIDC_AUTO_REGISTER = "false";
          OIDC_LINK_BY_USERNAME = "false";
        };
        serviceConfig = {
          User = "soulbeet";
          Group = "soulbeet";
          UMask = "0002";
          NoNewPrivileges = true;
          CapabilityBoundingSet = "";
          PrivateTmp = true;
          PrivateDevices = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ProtectKernelTunables = true;
          ProtectKernelModules = true;
          ProtectKernelLogs = true;
          ProtectControlGroups = true;
          RestrictSUIDSGID = true;
          RestrictRealtime = true;
          LockPersonality = true;
          RestrictAddressFamilies = [
            "AF_UNIX"
            "AF_INET"
            "AF_INET6"
          ];
          InaccessiblePaths = [
            "/nix/persist"
            "/var/lib"
            "/run/secrets"
            "-/run/secrets.d"
            "-/run/media-stack"
          ];
          EnvironmentFile = config.sops.secrets.soulbeet_env.path;
          WorkingDirectory = "${soulbeet}";
          ExecStart = getExe soulbeet;
          Restart = "on-failure";
          # the ui hands beets the host path, so it must be writable besides the /music bind
          ReadWritePaths = [ "/mnt/storage/media/my_music" ];
          BindPaths = [
            "${dataDir}:/data"
            "/mnt/storage/media/my_music:/music"
            "/mnt/storage/media/downloads/soulseek:/downloads"
          ];
        };
      };
  };
}
