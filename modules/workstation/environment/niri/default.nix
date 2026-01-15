{
  config,
  lib,
  pkgs,
  mlib,
  ...
}: let
  inherit (lib) mkIf getExe getExe' concatStrings;
  inherit (builtins) elem;
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf str;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;

  conf = config.meow.workstation.extraNiriConfig;
in {
  options = {
    meow.workstation.extraNiriConfig = mkOpt (listOf str) [] {};
  };

  config = mkIf (work && elem "niri" env) {
    environment.systemPackages = [pkgs.xwayland-satellite pkgs.swww];
    programs.niri.enable = true;

    xdg.portal.config = {
      common."org.freedesktop.impl.portal.OpenURI" = ["gtk"];
    };

    meow.home.modules = [
      {
        programs.waybar.enable = true;
        services.swaync.enable = true;
        services.vicinae.enable = true;
        programs.alacritty.enable = true;

        home.packages = [
          pkgs.nautilus
          pkgs.file-roller
          pkgs.blueberry
        ];

        xdg.configFile."niri/config.kdl".text = let
          colors = config.lib.stylix.colors.withHashtag;

          border = colors.base02;
          borderFocus = colors.base03;
          warn = colors.base0A;

          swayosd = getExe' pkgs.swayosd "swayosd-server";
          swayosdc = getExe' pkgs.swayosd "swayosd-client";
          waybar = getExe pkgs.waybar;
          swww = getExe' pkgs.swww "swww-daemon";
          xwayland-satellite = getExe pkgs.xwayland-satellite;
        in ''
          input {
            keyboard {
              xkb {
                layout "us,fi"
                options "grp:win_space_toggle"
              }

              repeat-delay 300
              repeat-rate 50
              track-layout "global"
            }

            touchpad {
              tap
              natural-scroll
            }

            mouse {
              accel-profile "flat"
            }

            focus-follows-mouse
            warp-mouse-to-focus
          }

          screenshot-path "~/Pictures/screenshots/%Y-%m-%d_%H-%M-%S.png"
          prefer-no-csd

          cursor {
            xcursor-theme "default"
            xcursor-size 24
          }

          environment {
            NIXOS_OZONE_WL "1"
          }

          overview {
            backdrop-color "${colors.base02}"
          }

          xwayland-satellite {
            path "${xwayland-satellite}"
          }

          hotkey-overlay {
            skip-at-startup
            hide-not-bound
          }

          gestures {
            hot-corners {
              top-left
            }
          }

          layout {
            gaps 16

            background-color "${colors.base00}"

            struts {
              left 0
              right 0
              top 0
              bottom 0
            }

            border {
              width 3
              active-color "${borderFocus}"
              inactive-color "${border}"
              urgent-color "${warn}"
            }
          }

          spawn-at-startup "${waybar}"
          spawn-at-startup "${swayosd}"
          spawn-at-startup "${swww}"

          recent-windows { off; }

          binds {
            Mod+E { spawn-sh "emacsclient -c -a '''"; }
            Mod+Return { spawn "alacritty"; }
            Mod+Semicolon { spawn-sh "emacsclient -c -a ''' -e '(meow/eshell)'"; }
            Mod+W { spawn "firefox"; }
            Print { screenshot; }

            End { spawn-sh "mumble rpc togglemute"; }

            Mod+D { spawn-sh "vicinae open"; }

            XF86AudioPlay { spawn-sh "mpc toggle"; }
            XF86AudioNext { spawn-sh "mpc next"; }
            XF86AudioPrev { spawn-sh "mpc prev"; }
            Mod+P { spawn-sh "mpc toggle"; }
            Mod+BracketRight { spawn-sh "mpc next"; }
            Mod+BracketLeft { spawn-sh "mpc prev"; }

            XF86AudioMute { spawn-sh "${swayosdc} --output-volume mute-toggle"; }
            XF86AudioRaiseVolume { spawn-sh "${swayosdc} --output-volume 3"; }
            XF86AudioLowerVolume { spawn-sh "${swayosdc} --output-volume -3"; }
            XF86MonBrightnessUp { spawn-sh "${swayosdc} --brightness=+5"; }
            XF86MonBrightnessDown { spawn-sh "${swayosdc} --brightness=-5"; }

            Mod+Q { close-window; }
            Mod+Shift+Q { quit; }

            Mod+T { toggle-column-tabbed-display; }

            Mod+F { maximize-column; }
            Mod+Shift+F { fullscreen-window; }

            Mod+H { focus-column-left; }
            Mod+J { focus-window-down; }
            Mod+K { focus-window-up; }
            Mod+L { focus-column-right; }
            Mod+Ctrl+H { focus-column-first; }
            Mod+Ctrl+L { focus-column-last; }
            Mod+Shift+Ctrl+H { move-column-to-first; }
            Mod+Shift+Ctrl+L { move-column-to-last; }

            Mod+Alt+J { focus-workspace-down; }
            Mod+Alt+K { focus-workspace-up; }
            Mod+Alt+Ctrl+J { focus-workspace 99; }
            Mod+Alt+Ctrl+K { focus-workspace 1; }

            Mod+Alt+Shift+J { move-window-to-workspace-down; }
            Mod+Alt+Shift+K { move-window-to-workspace-up; }
            Mod+Alt+Ctrl+Shift+J { move-window-to-workspace 99; }
            Mod+Alt+Ctrl+Shift+K { move-window-to-workspace 1; }

            Mod+Comma { focus-monitor-left; }
            Mod+Period { focus-monitor-right; }
            Mod+Shift+Comma { move-window-to-monitor-left; }
            Mod+Shift+Period { move-window-to-monitor-right; }

            Mod+1 { focus-workspace 1; }
            Mod+2 { focus-workspace 2; }
            Mod+3 { focus-workspace 3; }
            Mod+4 { focus-workspace 4; }
            Mod+5 { focus-workspace 5; }
            Mod+6 { focus-workspace 6; }
            Mod+7 { focus-workspace 7; }
            Mod+8 { focus-workspace 8; }
            Mod+9 { focus-workspace 9; }

            Mod+Shift+1 { move-window-to-workspace 1; }
            Mod+Shift+2 { move-window-to-workspace 2; }
            Mod+Shift+3 { move-window-to-workspace 3; }
            Mod+Shift+4 { move-window-to-workspace 4; }
            Mod+Shift+5 { move-window-to-workspace 5; }
            Mod+Shift+6 { move-window-to-workspace 6; }
            Mod+Shift+7 { move-window-to-workspace 7; }
            Mod+Shift+8 { move-window-to-workspace 8; }
            Mod+Shift+9 { move-window-to-workspace 9; }
          }

          ${concatStrings conf}
        '';

        # programs.niri.package = pkgs.niri;
        # programs.niri = {
        #   settings = {
        #     environment = {
        #       NIXOS_OZONE_WL = "1";
        #     };

        #     screenshot-path = "~/Pictures/screenshots/%Y-%m-%d_%H-%M-%S.png";
        #     prefer-no-csd = true;

        #     cursor.size = 24;

        #     input = {
        #       keyboard = {
        #         xkb = {
        #           layout = "us,fi";
        #           options = "grp:win_space_toggle";
        #         };

        #         repeat-delay = 300;
        #         repeat-rate = 50;
        #       };

        #       touchpad.tap = true;
        #       touchpad.tap-button-map = "left-right-middle";

        #       mouse.accel-profile = "flat";

        #       focus-follows-mouse.enable = true;
        #       warp-mouse-to-focus.enable = true;
        #     };

        #     gestures.hot-corners.enable = true;

        #     binds = {
        #       "Mod+W".action.spawn = "firefox";
        #       "Mod+Return".action.spawn = "alacritty";
        #       "Mod+E".action.spawn-sh = "emacsclient -c -a ''";
        #       "Mod+Semicolon".action.spawn-sh = "emacsclient -c -a '' -e '(meow/eshell)'";

        #       "Print".action.screenshot = {};
        #     };
        #   };
        # };
      }
    ];
  };
}
