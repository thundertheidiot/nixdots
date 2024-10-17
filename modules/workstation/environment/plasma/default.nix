{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (builtins) elem;
  inherit (lib) mkIf;
  inherit (mlib) mkEnOpt homeModule;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
  cfg = config.meow.workstation.plasma;
in {
  options = {
    meow.workstation.plasma = {
      vanilla = mkEnOpt "Don't configure plasma at all.";
      # TODO: kronkhite https://github.com/anametologin/krohnkite
      tiling = mkEnOpt "Configure plasma into a tiling environment.";
    };
  };

  config = mkIf (work && elem "plasma" env) (lib.mkMerge [
    {
      # Enable plasma
      services.desktopManager.plasma6 = {
        enable = true;
        enableQt5Integration = true;
      };

      # Package excludes
      environment.plasma6.excludePackages = with pkgs; [
        libsForQt5.elisa
      ];

      # xdg portal configuration
      # xdg.portal.extraPortals = [
      #   pkgs.kdePackages.xdg-desktop-portal-kde
      # ];

      xdg.portal.config = {
        kde = {
          default = ["kde"];
          "org.freedesktop.impl.portal.Secret" = ["gnome-keyring"];
          "org.freedesktop.impl.portal.Settings" = ["kde"];
        };
      };
    }
    {
      # Disable kwallet completely, we use gnome-keyring in every environment, to make switching between them not suck.
      # Swapping between kwallet and gnome-keyring breaks applications, and you need to relogin to places.
      environment.plasma6.excludePackages = with pkgs; [
        kdePackages.kwallet
        kdePackages.kwallet-pam
        kdePackages.kwalletmanager
      ];

      security.pam.services = {
        login.kwallet.enable = lib.mkForce false;
        kde.kwallet.enable = lib.mkForce false;
      };
    }
    (homeModule ({...}: (let
      V = val: {
        value = val;
        immutable = true;
      };
    in (lib.mkMerge [
      {
        programs.plasma.enable = true;
        # FIXME: wrangle emacs keybind
        # xdg.dataFile."applications/emacsclient-plasma.desktop" = {
        #   text = ''
        #     [Desktop Entry]
        #     Exec=emacsclient -c
        #     Name=emacsclient -c
        #     NoDisplay=true
        #     StartupNotify=false
        #     Type=Application
        #     X-KDE-GlobalAccel-CommandShortcut=true
        #   '';
        # };

        # programs.plasma = {
        #   configFile.kglobalshortcutsrc = {
        #     "services/org.kde.dolphin.desktop"."_launch" = V "";
        #     "useless/key-for-workaround.desktop"."_launch".value = "Meta+E";
        #     "services/emacsclient-plasma.desktop"."_launch" = V "Meta+E";
        #   };
        # };
      }
      {
        home.activation.plasmaPowerdevilSettings = ''
          run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Display --key DimDisplayWhenIdle false
          run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Display --key TurnOffDisplayWhenIdle false

          run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group Performance --key PowerProfile performance

          run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group AC --group SuspendAndShutdown --key AutoSuspendAction 0

          run ${pkgs.kdePackages.kconfig}/bin/kwriteconfig6 --file ~/.config/powerdevilrc --group Battery --group Performance --key PowerProfile power-saver
        '';
      }
      {
        # Keybinds
        programs.plasma = {
          hotkeys.commands = {
            "term" = {
              name = "Launch Terminal";
              key = "Meta+Return";
              command = "alacritty";
            };
            "web" = {
              name = "Launch Web Browser";
              key = "Meta+W";
              command = "firefox";
            };
            "mpd-toggle" = {
              name = "Toggle MPD";
              key = "Meta+P";
              command = "mpc toggle";
            };
            "restart-kwin" = {
              name = "Restart Kwin";
              key = "Meta+Shift+R";
              command = "kwin_wayland --replace";
            };
          };

          shortcuts = {
            # Removing conflicting defaults
            "kwin"."Overview" = "Meta";
            "kwin"."Show Desktop" = [];
            "services/org.kde.kscreen.desktop".ShowOSD = "Display";

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

              "Window Fullscreen" = "Meta+Shift+F";
              "Window Quick Tile Top" = [];
              "Window Maximize" = "Meta+Up";
            };

            "services/org.kde.krunner.desktop"._launch = "Meta+D";

            "KDE Keyboard Layout Switcher"."Switch to Next Keyboard Layout" = "Meta+Space";
          };
        };
      }
      {
        # Config
        programs.plasma.configFile = {
          "kcminputrc"."Keyboard" = {
            "RepeatDelay" = V 300;
            "RepeatRate" = V 50;
          };

          # Gnome keyring is used instead
          "kwalletrc" = {
            Wallet.Enabled = V false;
            "org.freedesktop.secrets"."apiEnabled" = V false;
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
          # "kwinrc"."ModifierOnlyShortcuts"."Meta" = V "org.kde.kglobalaccel,/component/kwin,org.kde.kglobalaccel.Component,invokeShortcut,Overview";

          "kwinrc"."org.kde.kdecoration2" = {
            "ButtonsOnLeft" = V "S";
            "ButtonsOnRight" = V "IAX";
          };
        };
      }
      (mkIf cfg.tiling {
        xdg.dataFile."kwin/scripts/krohnkite" = let
          krohnkite = pkgs.callPackage ./krohnkite.nix {};
        in {
          source = "${krohnkite}/share/kwin/scripts/krohnkite";
          recursive = true;
        };

        programs.plasma.configFile."kwinrc" = {
          "Plugins"."krohnkiteEnabled" = V true;
          "Script-krohnkite" = {
            "enableSpiralLayout" = V false;
            "enableColumnsLayout" = V false;
            "enableSpreadLayout" = V false;
            "enableStairLayout" = V false;

            "monocleMaximize" = V false;

            "screenGapBottom" = V 5;
            "screenGapLeft" = V 5;
            "screenGapRight" = V 5;
            "screenGapTop" = V 5;
            "tileLayoutGap" = V 5;

            "directionalKeyDwm" = V true;
            "directionalKeyFocus" = V false;

            # New window = top of stack
            "newWindowPosition" = V 1;
          };

          programs.plasma.configFile."kglobalshortcutsrc".kwin = {
            "KrohnkiteMonocleLayout" = V "Meta+F";
            "KrohnkiteTileLayout" = V "Meta+T";
            "KrohnkiteToggleFloat" = V "Meta+Shift+Space";
            "KrohnkiteSetMaster" = V "Meta+Shift+Return";

            "KrohnkiteShrinkWidth" = V "Meta+H";
            "KrohnkiteFocusPrev" = V "Meta+J";
            "KrohnkiteFocusNext" = V "Meta+K";
            "KrohnkitegrowWidth" = V "Meta+L"; # typo in krohnkite
          };
        };
      })
    ]))))
  ]);
}
