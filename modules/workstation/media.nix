# mpd, ncmpcpp, mpv
{
  config,
  pkgs,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOpt homeModule;
  inherit (lib) mkIf;
  cfg = config.meow.workstation.media.enable;
in {
  options = {
    meow.workstation.media.enable = mkEnOpt "Media players.";
  };

  config = mkIf cfg (homeModule
    ({config, ...}: {
      home.packages = with pkgs; [
        mpc-cli
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
        mpd = {
          host = "127.0.0.1";
          port = 6600;
          musicDirectory = config.services.mpd.musicDirectory;
        };
        # multimediaKeys = true;
        notifications = false;
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
    }));
}
