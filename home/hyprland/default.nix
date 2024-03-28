{
  config,
  pkgs,
  lib,
  localconfig,
  inputs,
  ...
}: let
  localStartup = pkgs.writeShellScriptBin "start" localconfig.hyprland.startup;

  colors = with config.scheme.withHashtag; {
    background = base00;
    foreground = base07;
    inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
  };

  colorsNoHash = with config.scheme; {
    background = base00;
    foreground = base07;
    inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
  };

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
    hyprctl="${inputs.hyprland.packages.${pkgs.system}.hyprland}/bin/hyprctl"
    wl-copy="${pkgs.wl-clipboard}/bin/wl-copy"

    [ "$1" = "region" ] && {
      "$grim" -g "$($slurp)" - | "$wl-copy" -t image/png
      exit 0
    }

    [ ! -d "$dir" ] && mkdir --parents "$dir"

    choice=$(printf "region\nregion save\nregion with annotation\noutput\noutput save\noutput with annotation" | "$bemenu")

    [ "$choice" = "region" ] && "$grim" -g "$($slurp)" - | "$wl-copy" -t image/png
    [ "$choice" = "region save" ] && "$grim" -g "$($slurp)" -t png "$dir/$date.png"
    [ "$choice" = "region with annotation" ] && "$grim" -g "$($slurp)" - | "$swappy" -f -

    [ "$choice" = "output" ] && {
      "$grim" -o "$($hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | "$bemenu")" - | "$wl-copy" -t image/png
    }

    [ "$choice" = "output save" ] && {
      "$grim" -o "$($hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | "$bemenu")" -t png "$dir/$date.png"
    }

    [ "$choice" = "output with annotation" ] && {
      "$grim" -o "$($hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | "$bemenu")" - | swappy -f -
    }
  '';
in {
  imports = [
    ./waybar.nix
  ];

  config = lib.mkIf (localconfig.install.hyprland) (with config; {
    home.packages = with pkgs; [
      hyprpaper
      grim
      slurp
      swappy
      bemenu
      wl-clipboard
    ];

    home.file.".config/hypr/hyprpaper.conf".text = ''
      preload = ~/.local/share/bg
      wallpaper = ,~/.local/share/bg
    '';

    home.file.".config/swappy/config".text = ''
      [Default]
      save_dir=${xdg.userDirs.pictures}/screenshots
      save_filename_format=annotated-%Y-%m-%d_%H-%M-%S.png
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
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;

      plugins = [
        inputs.split-monitor-workspaces.packages.${pkgs.system}.split-monitor-workspaces
      ];

      extraConfig = localconfig.hyprland.config;

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
          "${localStartup}/bin/start"
          "${pkgs.mako}/bin/mako"
          "${pkgs.hyprpaper}/bin/hyprpaper"
          "${pkgs.waybar}/bin/waybar"
          (lib.mkIf (localconfig.install.tv) "${pkgs.kodi} -fs")
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

        device = [
          {
            name = "tpps/2-ibm-trackpoint";
            accel_profile = "adaptive";
            # sensitivity = 0.0;
          }
          {
            name = "synps/2-synaptics-touchpad";
            accel_profile = "adaptive";
            sensitivity = 0.0;
          }
        ];

        misc = {
          disable_hyprland_logo = true;
          disable_splash_rendering = true;
          force_default_wallpaper = 0;
        };

        general = {
          gaps_in = 5;
          gaps_out = 20;
          border_size = 2;
          "col.active_border" = "rgb(${colorsNoHash.foreground})";
          "col.inactive_border" = "rgb(${colorsNoHash.background})";

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
          ",XF86AudioRaiseVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 3%+"
          ",XF86AudioLowerVolume, exec, ${pkgs.wireplumber}/bin/wpctl @DEFAULT_AUDIO_SINK@ 3%-"
        ];

        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];

        bind = [
          "$mod, return, exec, ${terminal}"
          "$mod, W, exec, ${pkgs.firefox}/bin/firefox"
          "$mod, D, exec, ${pkgs.bemenu}/bin/bemenu-run"
          "$mod, E, exec, $EDITOR"
          "$mod, M, exec, ${terminal} -e ${pkgs.ncmpcpp}/bin/ncmpcpp"
          "$shiftmod, M, exec, ${terminal} -e ${pkgs.pulsemixer}/bin/pulsemixer"
          "$mod, B, exec, ${terminal} -e ${pkgs.btop}/bin/btop"
          "$shiftmod, B, exec, ${terminal} -e ${pkgs.nvtopPackages.full}/bin/nvtop"

          ",XF86AudioPlay, exec, ${pkgs.mpd}/bin/mpc toggle"
          ",XF86AudioNext, exec, ${pkgs.mpd}/bin/mpc next"
          ",XF86AudioPrev, exec, ${pkgs.mpd}/bin/mpc prev"

          "$shiftmod, return, layoutmsg, swapwithmaster master"

          ",Print, exec, ${screenshot}/bin/screenshot region"
          "SHIFT, Print, exec, ${screenshot}/bin/screenshot"

          "$mod, Q, killactive"
          "$shiftmod, Q, exit"

          "$shiftmod, space, togglefloating"
          "$mod, F, fullscreen, 1"
          "$shiftmod, F, fullscreen"

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
  });
}
