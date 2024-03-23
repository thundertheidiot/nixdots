{
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}: let
  startup = pkgs.writeShellScriptBin "start" ''

  '';
  localStartup = pkgs.writeShellScriptBin "start" localconfig.hyprlandStartup;

  colors = with config.scheme.withHashtag; {
    background = base00;
    foreground = base07;
    inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
  };

  terminal = "${pkgs.alacritty}/bin/alacritty";
in
  with config; {
    home.packages = with pkgs; [
      hyprpaper
    ];

    home.file.".config/hypr/hyprpaper.conf".text = ''
      preload = ~/.local/share/bg
      wallpaper = ,~/.local/share/bg
    '';

    programs.bemenu = {
      enable = true;
      settings = {
        line-height = 30;
        ignorecase = true;
        tb = colors.background;
        tf = colors.foreground;
        fb = colors.background;
        ff = colors.foreground;
        cb = colors.foreground;
        cf = colors.foreground;
        nb = colors.background;
        nf = colors.foreground;
        hb = colors.background;
        hf = colors.base04;
        sb = colors.background;
        sf = colors.base04;
      };
    };

    services.mako = {
      enable = true;
      backgroundColor = "${colors.background}FF";
      borderColor = "${colors.foreground}FF";
      font = "monospace 10";
      margin = "10";
      padding = "5";
      borderSize = 2;
      borderRadius = 6;
      icons = true;
      maxIconSize = 32;
      defaultTimeout = 2000;
      ignoreTimeout = true;
    };

    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = true;
      xwayland.enable = true;

      plugins = [
        inputs.split-monitor-workspaces.packages.${pkgs.system}.split-monitor-workspaces
      ];

      extraConfig = localconfig.hyprlandConfig;

      settings = {
        "$mod" = "SUPER";
        "$shiftmod" = "SUPER_SHIFT";

        plugin.split-monitor-workspaces = {
          count = 10;
          keep_focused = 1;
        };

        windowrulev2 = [
          "workspace 9 silent,class:(steam)"
          "workspace 7 silent,class:(gajim)"
          "workspace 6 silent,class:(easyeffects)"

          "fullscreen,class:(cs2)"
          "stayfocused, title:^()$,class:^(steam)$"
        ];

        env = [
          "XCURSOR_SIZE,24"
          "XDG_CURRENT_DESKTOP,Hyprland"
          "XDG_SESSION_TYPE,wayland"
          "XDG_SESSION_DESKTOP,Hyprland"
        ];

        exec-once = [
          "${startup}/bin/start"
          "${localStartup}/bin/start"
          "${pkgs.mako}/bin/mako"
          "${pkgs.hyprpaper}/bin/hyprpaper"
        ];

        input = {
          kb_layout = "us,fi";
          kb_options = "grp:win_space_toggle";

          repeat_rate = 50;
          repeat_delay = 300;

          follow_mouse = 1;

          touchpad = {
            natural_scroll = true;
          };

          accel_profile = "flat";
          sensitivity = -0.5;
        };

        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
          force_default_wallpaper = 0;
        };

        general = {
          gaps_in = 5;
          gaps_out = 20;
          border_size = 2;
          col.active_border = "rgb(${colors.base04})";
          col.inactive_border = "rgb(${colors.foreground})";

          layout = "master";
        };

        decoration = {
          rounding = 6;

          blur = {
            enabled = true;
            size = 3;
            passes = 1;
          };

          drop_shadow = true;
          shadow_range = 4;
          shadow_render_power = 3;
          col.shadow = "rgba(${colors.background}ee)";
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
        ];

        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];

        bind = [
          "$mod, return, exec ${terminal}"
          "$mod, W, exec firefox"
          "$mod, D, exec ${pkgs.firefox}/bin/firefox"
          "$mod, D, exec ${pkgs.bemenu}/bin/bemenu"
          "$mod, E, exec $EDITOR"
          "$mod, M, exec ${terminal} -e ncmpcpp"
          "$mod, B, exec ${terminal} -e btop"
          "$shiftmod, B, exec ${terminal} -e nvtop"
          "$shiftmod, B, exec ${terminal} -e pulsemixer"

          "$shiftmod, return, layoutmsg, swapwithmaster master"

          "$mod, Q, killactive"
          "$shiftmod, Q, exit"

          "$mod, space, togglefloating"
          "$mod, F, fullscreen, 1"
          "$mod, F, fullscreen"

          "$mod, 1, split-workspace, 1"
          "$mod, 2, split-workspace, 2"
          "$mod, 3, split-workspace, 3"
          "$mod, 4, split-workspace, 4"
          "$mod, 5, split-workspace, 5"
          "$mod, 6, split-workspace, 6"
          "$mod, 7, split-workspace, 7"
          "$mod, 8, split-workspace, 8"
          "$mod, 9, split-workspace, 9"
          "$mod, 0, split-workspace, 0"

          "$shiftmod, 1, split-movetoworkspacesilent, 1"
          "$shiftmod, 2, split-movetoworkspacesilent, 2"
          "$shiftmod, 3, split-movetoworkspacesilent, 3"
          "$shiftmod, 4, split-movetoworkspacesilent, 4"
          "$shiftmod, 5, split-movetoworkspacesilent, 5"
          "$shiftmod, 6, split-movetoworkspacesilent, 6"
          "$shiftmod, 7, split-movetoworkspacesilent, 7"
          "$shiftmod, 8, split-movetoworkspacesilent, 8"
          "$shiftmod, 9, split-movetoworkspacesilent, 9"
          "$shiftmod, 0, split-movetoworkspacesilent, 0"

          "$mod, period, focusmonitor, r"
          "$mod, comma, focusmonitor, l"
          "$shiftmod, period, movewindow, mon:r"
          "$shiftmod, comma, movewindow, mon:l"
        ];
      };
    };
  }
