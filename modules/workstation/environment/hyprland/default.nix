{
  config,
  lib,
  pkgs,
  mlib,
  inputs,
  ...
}: let
  inherit (lib) mkIf;
  inherit (mlib) mkOpt;
  inherit (lib.types) listOf str;
  inherit (builtins) elem;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  options = {
    meow.workstation.hyprland = {
      extraConfig = mkOpt str "" {
        description = "Extra configuration to add to the hyprland config file.";
      };
      extraAutostart = mkOpt (listOf str) [] {
        description = "Extra entries to exec-once.";
      };
    };
  };

  imports = [
    ./waybar.nix
    ./nsxiv.nix
    ./hyprlock.nix
    ./swaync.nix
    ./anyrun.nix
  ];

  config = mkIf (work && elem "hyprland" env) {
    programs.hyprland.enable = true;
    programs.hyprland.withUWSM = true;

    services.displayManager.sessionPackages = with pkgs; let
      hypr = inputs.hyprland.packages.${pkgs.system};
    in [
      hypr.hyprland
    ];

    xdg.autostart.enable = true;
    xdg.portal.extraPortals = with pkgs; [
      xdg-desktop-portal
      xdg-desktop-portal-gtk
    ];

    xdg.portal.config = {
      common.default = ["hyprland" "gtk"];
      hyprland = {
        default = ["hyprland" "gtk"];
        "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
        "org.freedesktop.impl.portal.OpenURI" = ["gtk"];
      };
    };

    meow.home.modules = [
      (let
        cfg = config.meow.workstation.hyprland;
        hyprlandPackage = config.programs.hyprland.package;
      in
        {config, ...}: let
          terminal = "alacritty";

          screenshot = pkgs.mpkgs.screenshot;
        in {
          # Conflicts on other environments, so it's launched manually instead
          services.hyprpaper.enable = lib.mkForce false;

          xdg.configFile."hypr/hyprpaper.conf".text = ''
            preload = ~/.local/share/bg
            wallpaper = ,~/.local/share/bg
            splash = false
          '';

          home.packages = with pkgs; [
            swayosd

            gparted
            blueberry
            (pkgs.nemo-with-extensions.overrideAttrs (final: prev: {
              extensions = with pkgs; [nemo-fileroller];
            }))
            file-roller
          ];

          programs.hyprlux = {
            enable = true;

            systemd = {
              enable = true;
              target = "hyprland-session.target";
            };

            night_light = {
              enabled = false;
            };

            vibrance_configs = [
              {
                window_class = "csgo_linux64";
                window_title = "";
                strength = 50;
              }
            ];
          };

          programs.waybar.enable = true;
          programs.alacritty.enable = lib.mkDefault true;

          xdg.configFile."swappy/config".text = ''
            [Default]
            save_dir=${config.xdg.userDirs.pictures}/screenshots
            save_filename_format=annotated-%Y-%m-%d_%H-%M-%S.png
          '';

          wayland.windowManager.hyprland = {
            enable = true;
            systemd.enable = true;
            xwayland.enable = true;
            package = hyprlandPackage;

            extraConfig = cfg.extraConfig;

            settings = {
              "$mod" = "SUPER";
              "$shiftmod" = "SUPER_SHIFT";

              debug.disable_logs = false;
              # fix gamescope
              debug.full_cm_proto = true;

              ecosystem.no_update_news = true;

              workspace = [
                "f[1], gapsout:0, gapsin:0"
              ];

              windowrulev2 = [
                # "workspace 9 silent,class:(steam)"
                "workspace 7 silent,class:(gajim)"
                "workspace 6 silent,class:(easyeffects)"

                # "fullscreen,class:(cs2)"
                "immediate,class:(.gamescope-wrapped)"
                "stayfocused, title:^()$,class:^(steam)$"

                # remove borders when fullscreen
                "bordersize 0, flaoting:0, onworkspace:f[1]"
              ];

              env = [
                "XCURSOR_SIZE,24"
                "XDG_CURRENT_DESKTOP,Hyprland"
                "XDG_SESSION_TYPE,wayland"
                "XDG_SESSION_DESKTOP,Hyprland"

                # "EDITOR,emacsclient -c -a ''"

                "QT_QPA_PLATFORMTHEME,qt5ct"

                # "WLR_DRM_NO_ATOMIC,1"
              ];

              exec-once =
                [
                  # "${pkgs.swaynotificationcenter}/bin/swaync"
                  "${pkgs.hyprpaper}/bin/hyprpaper"
                  "${pkgs.waybar}/bin/waybar"
                  "${pkgs.swayosd}/bin/swayosd-server"
                ]
                ++ cfg.extraAutostart;

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

              render = {
                direct_scanout = true;
              };

              general = {
                layout = "master";

                allow_tearing = true;
              };

              master = {
                new_status = "master";
                new_on_top = true;
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

              # bindr = [
              #   "SUPER, SUPER_L, exec, anyrun"
              # ];

              bindm = [
                "$mod, mouse:272, movewindow"
                "$mod, mouse:273, resizewindow"
              ];

              bind = [
                "$mod, return, exec, ${terminal}"
                "$mod, W, exec, firefox"
                "$mod, D, exec, anyrun"
                "$mod, E, exec, emacsclient -c -a ''"
                "$mod, semicolon, exec, emacsclient -c -a '' -e '(th/eshell)'"
                "$mod, M, exec, ${terminal} -e ${pkgs.ncmpcpp}/bin/ncmpcpp"
                "$shiftmod, M, exec, ${terminal} -e ${pkgs.pulsemixer}/bin/pulsemixer"
                "$mod, B, exec, ${terminal} -e ${pkgs.btop}/bin/btop"
                "$shiftmod, B, exec, ${terminal} -e nvtop"

                "$mod, N, exec, swaync-client -op"

                ",XF86AudioPlay, exec, ${pkgs.mpc-cli}/bin/mpc toggle"
                ",XF86AudioNext, exec, ${pkgs.mpc-cli}/bin/mpc next"
                ",XF86AudioPrev, exec, ${pkgs.mpc-cli}/bin/mpc prev"
                "$mod,P, exec, ${pkgs.mpc-cli}/bin/mpc toggle"
                "$mod,bracketright, exec, ${pkgs.mpc-cli}/bin/mpc next"
                "$mod,bracketleft, exec, ${pkgs.mpc-cli}/bin/mpc prev"

                "$shiftmod, return, layoutmsg, swapwithmaster master"

                ",Print, exec, ${screenshot}/bin/screenshot -c"
                "SHIFT, Print, exec, ${screenshot}/bin/screenshot"

                # ",End, pass, ^(info\.mumble\.Mumble)$"
                # ",End, pass, ^(Mumble)$"
                # ",End, pass, ^(discord)$"

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
        })
    ];
  };
}
