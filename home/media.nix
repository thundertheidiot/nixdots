{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (mlib) mkEnOptTrue mkOpt;
  inherit (lib) mkIf mkMerge;
  inherit (lib.types)
    nullOr
    str
    ;
  cfg = config.mHome.desktop.common.media;
in
{
  options = {
    mHome.desktop.common.media.enable = mkEnOptTrue "Set up media players";

    mHome.desktop.common.media.scrobbling = {
      tokenFile = mkOpt (nullOr str) null {
        description = "File containing the ListenBrainz user token. Setting this enables scrobbling.";
      };
      usernameFile = mkOpt (nullOr str) null {
        description = "File containing the ListenBrainz (MusicBrainz ID) username.";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        mpc
      ];

      services.mpd = {
        enable = true;
        musicDirectory = "${config.xdg.userDirs.music}/mpd";
        network.listenAddress = "any";
        extraConfig = ''
          restore_paused "yes"

          audio_output {
                       type "pipewire"
                       name "pipewire"
          }
        '';
      };

      services.mpdris2 = {
        enable = true;
        settings = {
          Library.music_dir = config.services.mpd.musicDirectory;
          Connection = {
            host = "127.0.0.1";
            port = 6600;
          };
          Bling.notify = false;
        };
      };

      programs.ncmpcpp = {
        enable = true;
        settings = {
          ncmpcpp_directory = "${config.xdg.dataHome}/ncmpcpp";
          lyrics_directory = "${config.xdg.dataHome}/ncmpcpp/lyrics";
          media_library_primary_tag = "album_artist";
          # media_library_split_by_date = "no";
          media_library_hide_album_dates = "yes";
          mpd_host = "127.0.0.1";
          mpd_port = "6600";
        };
        bindings = [
          {
            key = "h";
            command = "previous_column";
          }
          {
            key = "l";
            command = "next_column";
          }
          {
            key = "j";
            command = "scroll_down";
          }
          {
            key = "k";
            command = "scroll_up";
          }

          {
            key = "=";
            command = "volume_up";
          }
          {
            key = "-";
            command = "volume_down";
          }

          {
            key = "y";
            command = "toggle_single";
          }
          {
            key = "u";
            command = "update_database";
          }
        ];
      };

      programs.mpv = {
        enable = true;
        config = {
          volume = 75;
          ytdl-format = "bestvideo[height<=1080]+bestaudio";
          hwdec = "auto-safe";
          vo = "gpu";
          profile = "gpu-hq";
        };
        scripts = with pkgs.mpvScripts; [
          sponsorblock-minimal
          quality-menu
        ];
        bindings = {
          "G" = "script-binding quality_menu/video_formats_toggle";
          "Alt+g" = "script-binding quality_menu/audio_formats_toggle";
          "Ctrl+r" = "script-binding quality_menu/reload";
        };
      };
    }

    (mkIf (cfg.scrobbling.tokenFile != null && cfg.scrobbling.usernameFile != null) (
      let
        # the HM mpdscribble module can only template the password, so we run our
        # own unit and substitute both secrets with replace-secret, like it does
        template = pkgs.writeText "mpdscribble.conf" ''
          log = -
          verbose = 1
          journal_interval = 600
          host = localhost
          port = 6600

          # official AudioScrobbler-protocol proxy (username = MusicBrainz ID, password = token)
          [listenbrainz]
          url = http://proxy.listenbrainz.org
          username = {{LISTENBRAINZ_USERNAME}}
          password = {{LISTENBRAINZ_PASSWORD}}
          journal = ${config.xdg.dataHome}/mpdscribble/listenbrainz.journal
        '';

        preStart = pkgs.writeShellApplication {
          name = "mpdscribble-pre-start";
          runtimeInputs = with pkgs; [
            replace-secret
            coreutils
          ];
          text = ''
            configFile="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/mpdscribble/mpdscribble.conf"
            mkdir -p "$(dirname "$configFile")"
            cp --no-preserve=mode,ownership -f ${template} "$configFile"
            replace-secret '{{LISTENBRAINZ_USERNAME}}' '${cfg.scrobbling.usernameFile}' "$configFile"
            replace-secret '{{LISTENBRAINZ_PASSWORD}}' '${cfg.scrobbling.tokenFile}' "$configFile"
          '';
        };
      in
      {
        systemd.user.services.mpdscribble = {
          Unit = {
            Description = "mpdscribble mpd scrobble client";
            After = [
              "network.target"
              "mpd.service"
            ];
          };
          Install.WantedBy = [ "default.target" ];

          Service = {
            ExecStartPre = "+${preStart}/bin/mpdscribble-pre-start";
            ExecStart = "${pkgs.mpdscribble}/bin/mpdscribble --no-daemon --conf %t/mpdscribble/mpdscribble.conf";
            RuntimeDirectory = "mpdscribble";
            RuntimeDirectoryMode = "700";
            StateDirectory = "mpdscribble";
          };
        };
      }
    ))
  ]);
}
