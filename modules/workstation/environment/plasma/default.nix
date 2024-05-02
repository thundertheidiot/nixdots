{
  system = {
    lib,
    config,
    pkgs,
    ...
  }:
    lib.mkIf (config.workstation.environment == "plasma")
    {
      services.desktopManager.plasma6 = {
        enable = true;
        enableQt5Integration = true;
      };
    };

  home = { lib, config, pkgs, ... }: lib.mkIf (config.workstation.environment == "plasma") (let
    V = val: {
      value = val;
      immutable = true;
    };
  in (lib.mkMerge [
    {
      xdg.dataFile."applications/emacsclient-plasma.desktop" = {
        text = ''
          [Desktop Entry]
          Exec=emacsclient -c
          Name=emacsclient -c
          NoDisplay=true
          StartupNotify=false
          Type=Application
          X-KDE-GlobalAccel-CommandShortcut=true
        '';
      };

      xdg.configFile."autostart/plasma-bind-emacsclient.desktop" = {
        text = ''
          [Desktop Entry]
          Exec=sh -c "${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file $XDG_CONFIG_HOME/kglobalshortcutsrc --group services --group emacsclient-plasma.desktop --key _launch Meta+E && qdbus org.kde.KWin /KWin reconfigure"
          Name=bind emacsclient
        '';
      };

      # Plasma manager just couldn't do it consistently for whatever reason.
      home.activation.plasmaEmacsBindingHack = ''
        run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/kglobalshortcutsrc --group services --group org.kde.dolphin.desktop --key _launch ""
        # run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/kglobalshortcutsrc --group services --group emacsclient-plasma.desktop --key _launch Meta+E
      '';

      home.activation.plasmaPowerdevilSettings = ''
        run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Display --key DimDisplayWhenIdle false
        run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Display --key TurnOffDisplayWhenIdle false

        run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Performance --key PowerProfile performance

        run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group SuspendAndShutdown --key AutoSuspendAction 0

        run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group Battery --group Performance --key PowerProfile power-saver
      '';

      programs.plasma = {
        enable = true;

        hotkeys.commands = {
          "term" = {
            name = "Launch Terminal";
            key = "Meta+Return";
            command = "konsole";
          };
          "web" = {
            name = "Launch Web Browser";
            key = "Meta+W";
            command = "firefox";
          };
        };

        shortcuts = {
          # Remove defaults
          plasmashell = {
            "manage activities" = [];
            "activate task manager entry 1" = [];
            "activate task manager entry 2" = [];
            "activate task manager entry 3" = [];
            "activate task manager entry 4" = [];
            "activate task manager entry 5" = [];
            "activate task manager entry 6" = [];
            "activate task manager entry 7" = [];
            "activate task manager entry 8" = [];
            "activate task manager entry 9" = [];
            "activate task manager entry 10" = [];
          };
          "kwin"."Overview" = [];

          kwin = {
            "Switch to Desktop 1" = "Meta+1";
            "Switch to Desktop 2" = "Meta+2";
            "Switch to Desktop 3" = "Meta+3";
            "Switch to Desktop 4" = "Meta+4";
            "Switch to Desktop 5" = "Meta+5";
            "Switch to Desktop 6" = "Meta+6";
            "Switch to Desktop 7" = "Meta+7";
            "Switch to Desktop 8" = "Meta+8";
            "Switch to Desktop 9" = "Meta+9";

            "Window to Desktop 1" = "Meta+!";
            "Window to Desktop 2" = "Meta+@";
            "Window to Desktop 3" = "Meta+#";
            "Window to Desktop 4" = "Meta+$";
            "Window to Desktop 5" = "Meta+%";
            "Window to Desktop 6" = "Meta+^";
            "Window to Desktop 7" = "Meta+&";
            "Window to Desktop 8" = "Meta+*";
            "Window to Desktop 9" = "Meta+(";
          };
        };

        configFile = {
          "kcminputrc"."Keyboard" = {
            "RepeatDelay" = V 300;
            "RepeatRate" = V 50;
          };

          "kdeglobals"."KDE"."SingleClick" = V false;
          "kwinrc"."Xwayland"."XwaylandEavesdrops".value = "modifiers";
          "kwinrc"."Windows" = {
            "DelayFocusInterval" = V 0;
            "FocusPolicy" = V "FocusFollowsMouse";
            "NextFocusPrefersMouse" = V true;
          };
          "kwinrc"."Desktops"."Number" = V 9;
          "kwinrc"."Desktops"."Rows" = V 1;
          # Activities
          "kwinrc"."ModifierOnlyShortcuts"."Meta" = V "org.kde.kglobalaccel,/component/kwin,org.kde.kglobalaccel.Component,invokeShortcut,Overview";

          "kwinrc"."org.kde.kdecoration2" = {
            "ButtonsOnLeft" = V "S";
            "ButtonsOnRight" = V "IAX";
          };
        };
      };
    }
    (lib.mkIf (config.workstation.plasma.tilingwm) (let
      polonium = (import ./polonium.nix) pkgs;
    in {
      xdg.dataFile."kwin/scripts/polonium" = {
        source = "${polonium}/share/kwin/scripts/polonium";
        recursive = true;
      };

      programs.plasma = {
        shortcuts = {
          # Remove
          ksmserver = {
            "Lock Session" = [];
          };
          "kwin"."Edit Tiles" = [];

          kwin = {
            "Window Close" = "Meta+Q";

            "PoloniumFocusAbove" = "Meta+K";
            "PoloniumFocusBelow" = "Meta+J";
            "PoloniumFocusLeft" = "Meta+H";
            "PoloniumFocusRight" = "Meta+L";

            "PoloniumSwitchHalf" = "Meta+T";
            "PoloniumSwitchMonocle" = "Meta+F";

            "Move Window One Screen to the Left" = "Meta+<";
            "Move Window One Screen to the Right" = "Meta+>";
            "Switch to Screen to the Left" = "Meta+,";
            "Switch to Screen to the Right" = "Meta+.";
          };
        };

        configFile = {
          "kwinrc"."Windows"."SeparateScreenFocus" = V true;
          "kwinrc"."Plugins" = {
            "poloniumEnabled" = V true;
          };
          "kwinrc"."Script-polonium" = {
            "TilePopups" = V false;
            "EngineType" = V 1;
            "Borders" = V 3;
            "InsertionPoint" = V 1;
          };
        };
      };
    }))
  ]));
}
