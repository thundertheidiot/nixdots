{
  config,
  infra,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    concatMapStringsSep
    head
    splitString
    toSentenceCase
    ;
  inherit (config.meow.impermanence) persist;
  inherit (config.meow.server.services.navidrome) port expose;
in
{
  config = {
    meow.server.services.navidrome = {
      port = 4533;
      expose = [ "navidrome.meowcloud.net" ];
      protect = [ "users" ];
      # kanidm login picks the navidrome user, subsonic clients keep their own auth
      proxy = target: {
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

        locations."~* ^/metrics(/|$)".return = "404";
      };
    };

    sops.secrets.navidrome_env = {
      owner = config.services.navidrome.user;
    };

    users.users.${config.services.navidrome.user}.extraGroups = [ "media" ];
    systemd.services.navidrome.serviceConfig.BindReadOnlyPaths = [ "/mnt/storage/media" ];
    systemd.services.navidrome.environment.TMPDIR = "${persist}/navidrome/tmp";
    systemd.tmpfiles.rules = [
      "d /mnt/storage/media/my_music 2775 root media -"
      "d ${persist}/navidrome/tmp 0700 navidrome navidrome -"
    ];

    services.navidrome =
      let
        inherit (config.meow.workstation) theme;
        themeName = concatMapStringsSep " " toSentenceCase (
          splitString "-" "${theme.family}-${theme.variant}"
        );
      in
      {
        enable = true;
        package = pkgs.callPackage ../../pkgs/navidrome-themed.nix {
          inherit themeName;
          inherit (theme.palette) roles;
        };
        openFirewall = false;
        environmentFile = config.sops.secrets.navidrome_env.path;
        settings = {
          Address = "0.0.0.0";
          Port = port;
          MusicFolder = "/mnt/storage/media/music";
          DataFolder = "${persist}/navidrome";
          BaseUrl = "https://${head expose}";
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
  };
}
