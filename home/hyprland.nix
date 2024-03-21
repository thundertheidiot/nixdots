{ pkgs, inputs, ... }: {
  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;
    xwayland.enable = true;

    plugins = [
      # inputs.split-monitor-workspaces.packages.${pkgs.system}.split-monitor-workspaces
      inputs.hyprland-plugins.packages.${pkgs.system}.hyprbars
    ];

    settings = {
      "$mod" = "SUPER";

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
    };
  };
}
