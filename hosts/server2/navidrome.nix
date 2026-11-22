{
  config,
  infra,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    getExe
    head
    concatStringsSep
    mapAttrsToList
    ;
  inherit (config.meow.dns) names;
  beetsPython =
    let
      mv = config.multiverse.instance;
      beetsPkgs = mv.at (mv.revOf "beets" "2.11.0");
    in
    beetsPkgs.python3.withPackages (p: [ p.beets ]);
  soulbeet = pkgs.mpkgs.soulbeet;
  themeName = "Madoromi Haru Yoru";
in
{
  config = {
    sops.secrets.navidrome_env = {
      owner = config.services.navidrome.user;
    };

    users.users.${config.services.navidrome.user}.extraGroups = [ "media" ];
    systemd.services.navidrome.serviceConfig.BindReadOnlyPaths = [ "/mnt/storage/media" ];
    systemd.services.navidrome.environment.TMPDIR = "${config.meow.impermanence.persist}/navidrome/tmp";
    systemd.tmpfiles.rules = [
      "d /mnt/storage/media/my_music 2775 root media -"
      "d ${config.meow.impermanence.persist}/navidrome/tmp 0700 navidrome navidrome -"
    ];

    services.navidrome = {
      enable = true;
      package =
        let
          inherit (import ../../modules/workstation/theme/palettes/madoromi.nix { variant = "haru-yoru"; })
            roles
            ;
          # catppuccin mocha hexes used by the upstream theme files
          swaps = {
            "#1e1e2e" = roles.background;
            "#181825" = roles.panel;
            "#313244" = roles.surface;
            "#45475a" = roles.border;
            "#9399b2" = roles.subtle;
            "#a6adc8" = roles.muted;
            "#cdd6f4" = roles.foreground;
            "#cba6f7" = roles.accent;
            "#89b4fa" = roles.info;
            "#f38ba8" = roles.error;
          };
          sedArgs = concatStringsSep " " (mapAttrsToList (from: to: "-e 's/${from}/${to}/gI'") swaps);
        in
        pkgs.navidrome.override {
          navidrome-unwrapped = pkgs.navidrome-unwrapped.overrideAttrs (old: {
            postPatch = old.postPatch + ''
              cd ui/src/themes
              sed ${sedArgs} \
                -e "s|catppuccinMocha.css.js|madoromi.css.js|" \
                -e "s|themeName: 'Catppuccin Mocha'|themeName: '${themeName}'|" \
                catppuccinMocha.js > madoromi.js
              sed ${sedArgs} catppuccinMocha.css.js > madoromi.css.js
              substituteInPlace index.js --replace-fail "export default {" \
                "import MadoromiTheme from './madoromi'
              export default {
                MadoromiTheme,"
              cd -
            '';
          });
        };
      openFirewall = false;
      environmentFile = config.sops.secrets.navidrome_env.path;
      settings = {
        Address = "0.0.0.0";
        MusicFolder = "/mnt/storage/media/music";
        DataFolder = "${config.meow.impermanence.persist}/navidrome";
        BaseUrl = "https://${head infra.services.navidrome.public}";
        DefaultTheme = themeName;

        # kanidm auth, local and vps
        ExtAuth.TrustedSources = "127.0.0.1/32,${infra.hosts.${infra.dns.edge}.wg}/32";

        Agents = "deezer,lastfm,listenbrainz";

        LastFM.Enabled = true;
        Deezer.Enabled = true;
        ListenBrainz.Enabled = true;

        # needed by navidrome-mpd-sync, applies to newly registered players
        Subsonic.DefaultReportRealPath = true;
      };
    };

    # kanidm login picks the navidrome user, subsonic clients keep their own auth
    meow.kanidm.protect.${names.navidrome} = [ "users" ];
    services.nginx.virtualHosts.${names.navidrome} =
      let
        target = "http://127.0.0.1:${toString config.services.navidrome.settings.Port}";
      in
      {
        forceSSL = true;
        locations."/" = {
          proxyPass = target;
          recommendedProxySettings = true;
          proxyWebsockets = true;
          extraConfig = ''
            auth_request_set $username $upstream_http_x_auth_request_preferred_username;
            proxy_set_header Remote-User $username;
          '';
        };
        locations."/rest/" = {
          proxyPass = target;
          recommendedProxySettings = true;
          extraConfig = ''
            auth_request off;
            proxy_set_header Remote-User "";
          '';
        };
      };

    services.nginx.virtualHosts.${names.soulbeet} = {
      forceSSL = true;
      locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:9765";
          recommendedProxySettings = true;
          proxyWebsockets = true;
          extraConfig = ''
            proxy_set_header Upgrade $http_upgrade;
            proxy_set_header Connection "Upgrade";
            proxy_read_timeout 3600s;
            proxy_send_timeout 3600s;
          '';
        };
      };
    };

    users.groups.soulbeet = { };
    users.users.soulbeet = {
      isSystemUser = true;
      group = "soulbeet";
      extraGroups = [ "media" ];
    };

    systemd.services.soulbeet = {
      description = "Soulbeet";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network-online.target"
      ];
      wants = [ "network-online.target" ];
      unitConfig.RequiresMountsFor = [
        "/nix/persist/media-stack/soulbeet"
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
        PORT = "9765";
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
          "/nix/persist/media-stack/soulbeet:/data"
          "/mnt/storage/media/my_music:/music"
          "/mnt/storage/media/downloads/soulseek:/downloads"
        ];
      };
    };
  };
}
