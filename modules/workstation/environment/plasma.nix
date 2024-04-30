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

  home = {
    lib,
    config,
    pkgs,
    ...
  }:
    lib.mkIf (config.workstation.environment == "plasma") (let
      V = val: {
        value = val;
        immutable = true;
      };

      plasma-window-decorations = pkgs.stdenv.mkDerivation {
        name = "plasma-window-decorations";

        src = pkgs.fetchgit {
          url = "https://github.com/nclarius/Plasma-window-decorations";
          rev = "02058699173f5651816d4cb31960d08b45553255";
          hash = "sha256-O4JTtj/q2lJRhWS+nhfQes8jitkrfsSBmENHZb5ioNI=";
        };

        installPhase = ''
          mkdir --parents "$out/share/aurorae/themes"
          cp -r "$src/ActiveAccentFrame" "$out/share/aurorae/themes/ActiveAccentFrame"
        '';
      };

      polonium = pkgs.buildNpmPackage {
        pname = "polonium";
        version = "1.0.0";

        src = pkgs.fetchgit {
          url = "https://github.com/zeroxoneafour/polonium";
          rev = "59f232475cd1ce9453657b5c2cff63fc4b911c3b";
          hash = "sha256-65w/eyD4xIOLziK+Y6Mvg2RQLfQZIt/jbWyR63BSUiI=";
        };

        npmDepsHash = "sha256-kaT3Uyq+/JkmebakG9xQuR4Kjo7vk6BzI1/LffOj/eo=";

        dontConfigure = true;

        buildFlags = ["res" "src"];

        nativeBuildInputs = [pkgs.libsForQt5.plasma-framework];
        dontNpmBuild = true;

        dontWrapQtApps = true;

        installPhase = ''
          runHook preInstall

          plasmapkg2 --install pkg --packageroot $out/share/kwin/scripts

          runHook postInstall
        '';
      };
    in {
      xdg.dataFile."kwin/scripts/polonium" = {
        source = "${polonium}/share/kwin/scripts/polonium";
        recursive = true;
      };

      xdg.dataFile."aurorae/themes/ActiveAccentFrame" = {
        source = "${plasma-window-decorations}/share/aurorae/themes/ActiveAccentFrame";
        recursive = true;
      };

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
          "emacs" = {
            name = "Launch Emacs";
            key = "Meta+E";
            command = "emacsclient -c";
          };
        };

        shortcuts = {
          "services/org.kde.dolphin.desktop"."_launch" = [];
          ksmserver = {
            "Lock Session" = [];
          };
          kwin = {
            "Window Close" = "Meta+Q";

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
            "SeparateScreenFocus" = V true;
          };
          "kwinrc"."Desktops"."Number" = V 9;
          # Activities
          "kwinrc"."ModifierOnlyShortcuts"."Meta" = V "org.kde.kglobalaccel,/component/kwin,org.kde.kglobalaccel.Component,invokeShortcut,Overview";

          "kwinrc"."org.kde.kdecoration2" = {
            "ButtonsOnLeft" = V "S";
            "ButtonsOnRight" = V "IAX";
          };

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
    });
}
