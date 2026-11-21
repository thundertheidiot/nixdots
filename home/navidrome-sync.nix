{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (mlib) mkOpt;
  inherit (lib) mkIf getExe;
  inherit (lib.types) nullOr str attrsOf;
  inherit (builtins) toJSON readFile;
  cfg = config.mHome.desktop.common.media.navidromeSync;
in
{
  options.mHome.desktop.common.media.navidromeSync = {
    url = mkOpt str "https://navidrome.nya.meowcloud.net" { };
    username = mkOpt str "thunder" { };
    passwordFile = mkOpt (nullOr str) null {
      description = "File containing the navidrome password. Setting this enables playlist syncing.";
    };
    libraries =
      mkOpt (attrsOf str)
        {
          "/mnt/storage/media/music" = "vgm";
          "/mnt/storage/media/my_music" = "my_music";
          "/mnt/storage/media/metal" = "metal";
          "/mnt/storage/media/breakcore" = "breakcore";
          "/mnt/storage/media/japan" = "japan";
        }
        {
          description = "Navidrome library path to mpd music directory subpath.";
        };
  };

  config = mkIf (config.mHome.desktop.common.media.enable && cfg.passwordFile != null) (
    let
      script = pkgs.writers.writePython3Bin "navidrome-mpd-sync" {
        flakeIgnore = [ "E501" ];
      } (readFile ./navidrome-sync.py);

      syncConfig = pkgs.writeText "navidrome-mpd-sync.json" (toJSON {
        inherit (cfg)
          url
          username
          passwordFile
          libraries
          ;
        playlistDir = config.services.mpd.playlistDirectory;
        stateFile = "${config.xdg.stateHome}/navidrome-mpd-sync/state.json";
      });

      # wrapper so --dry-run can be run by hand
      wrapper = pkgs.writeShellScriptBin "navidrome-mpd-sync" ''
        exec ${getExe script} ${syncConfig} "$@"
      '';
    in
    {
      home.packages = [ wrapper ];

      systemd.user.services.navidrome-mpd-sync = {
        Unit = {
          Description = "Sync mpd and navidrome playlists";
          After = [ "network-online.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = getExe wrapper;
        };
      };

      systemd.user.timers.navidrome-mpd-sync = {
        Unit.Description = "Sync mpd and navidrome playlists";
        Timer = {
          OnStartupSec = "2min";
          OnUnitActiveSec = "10min";
        };
        Install.WantedBy = [ "timers.target" ];
      };
    }
  );
}
