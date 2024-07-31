{
  system = {
    lib,
    config,
    pkgs,
    inputs,
    ...
  }:
    lib.mkIf (builtins.elem "hyprland" config.workstation.environment) {
      programs.hyprland = {
        enable = true;
      };

      services.displayManager.sessionPackages = with pkgs; [
        hyprland
      ];
    };

  # Anything affected by monitor configuration is defined in modules/monitor.nix
  home = {
    mlib,
    lib,
    config,
    pkgs,
    inputs,
    ...
  }:
    lib.mkIf (builtins.elem "hyprland" config.workstation.environment) (
      let
        colorsNoHash = mlib.colorsNoHash config;

        terminal = "${pkgs.alacritty}/bin/alacritty";

        screenshot = pkgs.writeShellScriptBin "screenshot" ''
          #!/bin/sh
          . "$XDG_CONFIG_HOME/user-dirs.dirs"

          date="$(date +%Y-%m-%d_%H-%M-%S)"
          dir="${config.xdg.userDirs.pictures}/screenshots"

          bemenu="${pkgs.bemenu}/bin/bemenu"
          grim="${pkgs.grim}/bin/grim"
          slurp="${pkgs.slurp}/bin/slurp"
          swappy="${pkgs.swappy}/bin/swappy"
          hyprctl="${pkgs.hyprland}/bin/hyprctl"
          wlcopy="${pkgs.wl-clipboard}/bin/wl-copy"

          [ "$1" = "region" ] && {
            "$grim" -g "$($slurp)" - | "$wlcopy" -t image/png
            exit 0
          }

          [ ! -d "$dir" ] && mkdir --parents "$dir"

          choice=$(printf "region\nregion save\nregion with annotation\noutput\noutput save\noutput with annotation" | "$bemenu")

          [ "$choice" = "region" ] && "$grim" -g "$($slurp)" - | "$wlcopy" -t image/png
          [ "$choice" = "region save" ] && "$grim" -g "$($slurp)" -t png "$dir/$date.png"
          [ "$choice" = "region with annotation" ] && "$grim" -g "$($slurp)" - | "$swappy" -f -

          [ "$choice" = "output" ] && {
            "$grim" -o "$($hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | "$bemenu")" - | "$wlcopy" -t image/png
          }

          [ "$choice" = "output save" ] && {
            "$grim" -o "$($hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | "$bemenu")" -t png "$dir/$date.png"
          }

          [ "$choice" = "output with annotation" ] && {
            "$grim" -o "$($hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | "$bemenu")" - | swappy -f -
          }
        '';
      in {
        home.packages = with pkgs; [
          screenshot
          hyprpaper
        ];

        home.file.".config/hypr/hyprpaper.conf".text = ''
          preload = ~/.local/share/bg
          wallpaper = ,~/.local/share/bg
          splash = false
        '';

        wayland.windowManager.hyprland = {
          enable = true;
          systemd.enable = true;
          xwayland.enable = true;
          package = pkgs.hyprland;

          extraConfig = config.setup.hyprland.extraConfig;

          settings = {
            "$mod" = "SUPER";
            "$shiftmod" = "SUPER_SHIFT";

            debug.disable_logs = false;

            windowrulev2 = [
              # "workspace 9 silent,class:(steam)"
              "workspace 7 silent,class:(gajim)"
              "workspace 6 silent,class:(easyeffects)"

              "fullscreen,class:(cs2)"
              "immediate,class:(.gamescope-wrapped)"
              "stayfocused, title:^()$,class:^(steam)$"
            ];

            env = [
              "XCURSOR_SIZE,24"
              "XDG_CURRENT_DESKTOP,Hyprland"
              "XDG_SESSION_TYPE,wayland"
              "XDG_SESSION_DESKTOP,Hyprland"

              # "WLR_DRM_NO_ATOMIC,1"
            ];

            exec-once =
              [
                "${pkgs.mako}/bin/mako"
                "${pkgs.hyprpaper}/bin/hyprpaper"
                "${pkgs.waybar}/bin/waybar"
                "${pkgs.swayosd}/bin/swayosd-server"
              ]
              ++ config.setup.hyprland.extraAutostart;

            input = {
              kb_layout = "us,fi";
              kb_options = "grp:win_space_toggle";

              repeat_rate = 50;
              repeat_delay = 300;

              follow_mouse = 1;

              touchpad = {
                natural_scroll = true;
                disable_while_typing = true;
              };

              accel_profile = "flat";
              sensitivity = 0.0;
            };

            device = [
              {
                name = "tpps/2-ibm-trackpoint";
                accel_profile = "adaptive";
                sensitivity = 0.0;
              }
              {
                name = "synps/2-synaptics-touchpad";
                accel_profile = "adaptive";
                sensitivity = 0.0;
              }
              {
                name = "synaptics-tm3053-004";
                accel_profile = "adaptive";
                sensitivity = -0.3;
              }
            ];

            misc = {
              disable_hyprland_logo = true;
              force_default_wallpaper = 0;
              vrr = 0;
            };

            general = {
              gaps_in = 5;
              gaps_out = 20;
              border_size = 2;
              "col.active_border" = "rgb(${colorsNoHash.foreground})";
              "col.inactive_border" = "rgb(${colorsNoHash.background})";

              layout = "master";

              allow_tearing = true;
            };

            decoration = {
              rounding = 6;

              blur = {
                enabled = false;
                size = 3;
                passes = 1;
              };

              drop_shadow = true;
              shadow_range = 4;
              shadow_render_power = 3;
              "col.shadow" = "rgba(${colorsNoHash.background}ee)";
            };

            animations.enabled = false;

            master = {
              new_is_master = true;
              mfact = 0.5;
            };

            binde = [
              "$mod, H, splitratio, -0.1"
              "$mod, J, layoutmsg, cyclenext"
              "$mod, K, layoutmsg, cycleprev"
              "$mod, L, splitratio, +0.1"
              ",XF86AudioMute, exec, ${pkgs.swayosd}/bin/swayosd-client --output-volume mute-toggle"
              ",XF86AudioRaiseVolume, exec, ${pkgs.swayosd}/bin/swayosd-client --output-volume 3"
              ",XF86AudioLowerVolume, exec, ${pkgs.swayosd}/bin/swayosd-client --output-volume -3"
              ",XF86MonBrightnessUp, exec, ${pkgs.swayosd}/bin/swayosd-client --brighness raise"
              ",XF86MonBrightnessDown, exec, ${pkgs.swayosd}/bin/swayosd-client --brighness lower"
            ];

            bindm = [
              "$mod, mouse:272, movewindow"
              "$mod, mouse:273, resizewindow"
            ];

            bind = [
              "$mod, return, exec, ${terminal}"
              "$mod, W, exec, ${pkgs.firefox}/bin/firefox"
              "$mod, D, exec, $(${pkgs.tofi}/bin/tofi-run)"
              "$mod, E, exec, $EDITOR"
              "$mod, M, exec, ${terminal} -e ${pkgs.ncmpcpp}/bin/ncmpcpp"
              "$shiftmod, M, exec, ${terminal} -e ${pkgs.pulsemixer}/bin/pulsemixer"
              "$mod, B, exec, ${terminal} -e ${pkgs.btop}/bin/btop"
              "$shiftmod, B, exec, ${terminal} -e nvtop"

              ",XF86AudioPlay, exec, ${pkgs.mpc-cli}/bin/mpc toggle"
              ",XF86AudioNext, exec, ${pkgs.mpc-cli}/bin/mpc next"
              ",XF86AudioPrev, exec, ${pkgs.mpc-cli}/bin/mpc prev"
              "$mod,P, exec, ${pkgs.mpc-cli}/bin/mpc toggle"
              "$mod,bracketright, exec, ${pkgs.mpc-cli}/bin/mpc next"
              "$mod,bracketleft, exec, ${pkgs.mpc-cli}/bin/mpc prev"

              "$shiftmod, return, layoutmsg, swapwithmaster master"

              ",Print, exec, ${screenshot}/bin/screenshot region"
              "SHIFT, Print, exec, ${screenshot}/bin/screenshot"

              ",End, pass, ^(info\.mumble\.Mumble)$"
              ",End, pass, ^(Mumble)$"
              ",End, pass, ^(discord)$"

              "$mod, Q, killactive"
              "$shiftmod, Q, exit"

              "$shiftmod, space, togglefloating"
              "$mod, F, fullscreen, 1"
              "$shiftmod, F, fullscreen"

              "$mod, period, focusmonitor, r"
              "$mod, comma, focusmonitor, l"
              "$shiftmod, period, movewindow, mon:r"
              "$shiftmod, comma, movewindow, mon:l"
            ];
          };
        };
      }
    );
}
