{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (builtins) elem;
  inherit (lib) mkIf;
  inherit (mlib) mkEnOpt mkEnOptTrue homeModule;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
  cfg = config.meow.workstation.plasma;

  # plasma manager helper
  F = val: {
    value = val;
    immutable = true;
  };
in {
  options = {
    meow.workstation.plasma = {
      basicConfig = mkEnOptTrue "Configure basic plasma settings.";
      opinionatedConfig = mkEnOpt "Configure more opinionated settings.";
    };
  };

  config = mkIf (work && elem "plasma" env) (lib.mkMerge [
    {
      # Enable plasma
      services.desktopManager.plasma6 = {
        enable = true;
        enableQt5Integration = true;
      };
    }
    # Wrangle kwallet + gnome-keyring config with working ssh-agent
    {
      services.gnome.gcr-ssh-agent.enable = false;
      programs.ssh = {
        startAgent = true;
        enableAskPassword = true;
      };

      xdg.portal.config = {
        kde = {
          default = ["kde"];
          "org.freedesktop.impl.portal.Secret" = ["gnome-keyring" "kwallet"];
          "org.freedesktop.impl.portal.Settings" = ["kde" "gtk"];
        };
      };

      environment.variables = {
        SSH_ASKPASS_REQUIRE = "prefer";
      };

      home-manager.sharedModules = [
        {
          # Backwards compatibility, these were previously forced to false, this will upgrade old configurations
          programs.plasma.configFile."kwalletrc" = {
            Wallet.Enabled = F true;
            "org.freedesktop.secrets"."apiEnabled" = F true;
          };
        }
      ];
    }
    (homeModule ({...}: (let
    in (lib.mkMerge [
      {
        programs.plasma.enable = true;
      }
      (mkIf cfg.basicConfig {
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
      })
      (mkIf cfg.opinionatedConfig {
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
        #     "services/org.kde.dolphin.desktop"."_launch" = F "";
        #     "useless/key-for-workaround.desktop"."_launch".value = "Meta+E";
        #     "services/emacsclient-plasma.desktop"."_launch" = F "Meta+E";
        #   };
        # };
      })
      (mkIf cfg.opinionatedConfig {
        # Keybinds
        programs.plasma = {
          configFile = {
            "kcminputrc"."Keyboard" = {
              "RepeatDelay" = F 300;
              "RepeatRate" = F 50;
            };

            "kwinrc"."Desktops"."Number" = F 9;
            "kwinrc"."Desktops"."Rows" = F 1;
            "kwinrc"."Plugins"."shakecursorEnabled" = F false;
          };

          shortcuts = {
            # Removing conflicting defaults
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
          };
        };
      })
    ]))))
  ]);
}
