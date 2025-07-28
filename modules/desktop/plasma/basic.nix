{config, ...}: let
  inherit (config.flake.lib.plasma) F;
in {
  flake.modules.homeManager."plasma/basic" = {pkgs, ...}: {
    programs.plasma.enable = true;

    home.activation.plasmaPowerdevilSettings = ''
      run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Display --key DimDisplayWhenIdle false
      run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Display --key TurnOffDisplayWhenIdle false

      run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Performance --key PowerProfile performance

      run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group SuspendAndShutdown --key AutoSuspendAction 0

      run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group Battery --group Performance --key PowerProfile power-saver
    '';

    programs.plasma = {
      shortcuts = {
        "kwin"."Overview" = "Meta";
        "kwin"."Show Desktop" = [];
        "services/org.kde.kscreen.desktop".ShowOSD = "Display";

        kwin = {
          "Window Quick Tile Top" = [];
          "Window Maximize" = "Meta+Up";
        };

        "KDE Keyboard Layout Switcher"."Switch to Next Keyboard Layout" = "Meta+Space";
      };

      configFile = {
        "kdeglobals"."KDE"."SingleClick" = F false;
        "kwinrc"."Xwayland"."XwaylandEavesdrops".value = "modifiers";
        "kwinrc"."Plugins"."shakecursorEnabled" = F false;
        "kwinrc"."Windows" = {
          "DelayFocusInterval" = F 0;
          "FocusPolicy" = F "FocusFollowsMouse";
          "NextFocusPrefersMouse" = F true;
        };

        "kwinrc"."org.kde.kdecoration2" = {
          "ButtonsOnLeft" = F "S";
          "ButtonsOnRight" = F "IAX";
        };
      };
    };
  };
}
