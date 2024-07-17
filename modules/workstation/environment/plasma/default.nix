{
  system = {
    lib,
    config,
    pkgs,
    ...
  }:
    lib.mkIf (builtins.elem "plasma" config.workstation.environment)
    {
      services.desktopManager.plasma6 = {
        enable = true;
        enableQt5Integration = true;
      };

      environment.plasma6.excludePackages = with pkgs; [
        libsForQt5.elisa
        kdePackages.kwallet
        kdePackages.kwallet-pam
      ];

      security.pam.services = {
        login.kwallet.enable = lib.mkForce false;
        kde.kwallet.enable = lib.mkForce false;
      };

      # use gnome-keyring everywhere, because switching between gnome-keyring and kwallet constantly seems to break some things
      # gnome-keyring seems to work better i think
    };

  home = {
    lib,
    config,
    pkgs,
    ...
  }:
    lib.mkIf (builtins.elem "plasma" config.workstation.environment) (
      let
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

          programs.plasma = {
            configFile.kglobalshortcutsrc = {
              "services/org.kde.dolphin.desktop"."_launch" = V "";
              "useless/key-for-workaround.desktop"."_launch".value = "Meta+E";
              "services/emacsclient-plasma.desktop"."_launch" = V "Meta+E";
            };
          };
        }
        {
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
                command = "alacritty";
              };
              "web" = {
                name = "Launch Web Browser";
                key = "Meta+W";
                command = "firefox";
              };
              "restart-kwin" = {
                name = "Restart Kwin";
                key = "Meta+Shift+R";
                command = "kwin_wayland --replace";
              };
            };

            shortcuts = {
              # Removing conflicting defaults
              "kwin"."Overview" = [];
              "kwin"."Show Desktop" = [];

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
              };

              "services/org.kde.krunner.desktop"._launch = "Meta+D";

              "KDE Keyboard Layout Switcher"."Switch to Next Keyboard Layout" = "Meta+Space";
            };

            configFile = {
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
              "kwinrc"."ModifierOnlyShortcuts"."Meta" = V "org.kde.kglobalaccel,/component/kwin,org.kde.kglobalaccel.Component,invokeShortcut,Overview";

              "kwinrc"."org.kde.kdecoration2" = {
                "ButtonsOnLeft" = V "S";
                "ButtonsOnRight" = V "IAX";
              };
            };
          };
        }
        (lib.mkIf (config.workstation.plasma.tilingwm) (
          let
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

                  "PoloniumRetileWindow" = "Meta+Shift+Space";

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
          }
        ))
      ])
    );
}
