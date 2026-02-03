{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (lib) mkIf;
  inherit (builtins) elem;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "hyprland" env) {
    programs.hyprlock.enable = true;
    services.hypridle.enable = true;

    meow.home.modules = [
      {
        services.hypridle = {
          enable = true;
          settings = {
            general = {
              lock_cmd = "pidof hyprlock >/dev/null || hyprlock";
              before_sleep_cmd = "loginctl lock-session";
              after_sleep_cmd = "hyprctl dispatch dpms on";
            };

            auth = {
              fingerprint.enabled = true;
            };

            # 5 min: lock. 10 min: screen off. 60 min: suspend.
            listener = [
              {
                timeout = 300;
                "on-timeout" = "loginctl lock-session";
                "on-resume" = "hyprctl dispatch dpms on";
              }
              {
                timeout = 600;
                "on-timeout" = "hyprctl dispatch dpms off";
                "on-resume" = "hyprctl dispatch dpms on";
              }
              {
                timeout = 3600;
                "on-timeout" = "systemctl suspend";
              }
            ];
          };
        };

        programs.hyprlock = {
          enable = true;

          settings = {
            general = {
              disable_loading_bar = true;
              grace = 300;
              hide_cursor = true;
              no_fade_in = false;
            };

            background = [
              {
                path = "~/.local/share/bg";
                blur_passes = 3;
                blur_size = 8;
              }
            ];

            label = [
              {
                text = ''cmd[update:1000] date "+%H:%M"'';
                font_size = 120;
                position = "0, -10";
                halign = "center";
                valign = "center";
              }
              {
                text = ''cmd[update:60000] date "+%A • %b %d"'';
                font_size = 18;
                position = "0, 70";
                halign = "center";
                valign = "center";
              }
            ];

            "input-field" = [
              {
                size = "280, 60";
                position = "0, -140";
                rounding = 12;
                dots_center = true;
                fade_on_empty = false;

                placeholder_text = "<i>Type password…</i>";
                font_color = "rgb(230,230,230)";
                inner_color = "rgba(0,0,0,0.35)";
                outer_color = "rgba(255,255,255,0.20)";
                outline_thickness = 2;
              }
            ];
          };
        };
      }
    ];
  };
}
