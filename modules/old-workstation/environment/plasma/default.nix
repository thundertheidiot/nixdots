# TODO: move to workstation
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

      # use gnome-keyring everywhere, because switching between gnome-keyring and kwallet constantly seems to break some things
      # gnome-keyring seems to work better i think
      environment.plasma6.excludePackages = with pkgs; [
        libsForQt5.elisa
        kdePackages.kwallet
        kdePackages.kwallet-pam
        kdePackages.kwalletmanager
      ];

      security.pam.services = {
        login.kwallet.enable = lib.mkForce false;
        kde.kwallet.enable = lib.mkForce false;
      };
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
        }
        {
          programs.plasma = {
            enable = true;

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
              # "kwinrc"."ModifierOnlyShortcuts"."Meta" = V "org.kde.kglobalaccel,/component/kwin,org.kde.kglobalaccel.Component,invokeShortcut,Overview";

              "kwinrc"."org.kde.kdecoration2" = {
                "ButtonsOnLeft" = V "S";
                "ButtonsOnRight" = V "IAX";
              };
            };
          };
        }
        (lib.mkIf (config.workstation.plasma.tilingwm) (
          let
            #TODO: update
            # TODO: kronkhite or the other extension ?
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
