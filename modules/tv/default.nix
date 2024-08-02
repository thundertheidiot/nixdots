{
  config,
  pkgs,
  inputs,
  lib,
  mlib,
  mpkgs,
  ...
}: let
  cfg = config.meow.tv;
  inherit (mlib) mkEnOpt;
in {
  options = {
    meow.tv.enable = mkEnOpt "Enable configuration for a \"smart tv\" system.";
  };

  config = lib.mkIf cfg.enable (let
    inherit (pkgs) callPackage;

    kodiPackage = callPackage mpkgs.kodi {};
    kodiHome = "${config.meow.home.directory}/.local/share/kodi";
    kodiExecutable = "${kodiPackage}/bin/kodi_with_addons";

    kodiSettings = callPackage ./kodi/settings.nix {
      inherit kodiHome;
    };
  in {
    # TODO stop assuming hyprland
    services.displayManager.sddm = {
      settings = {
        Autologin = {
          Session = "hyprland.desktop";
          User = "${config.username}";
        };
      };
    };

    systemd.services."ir-client" = let
      naersk = pkgs.callPackage inputs.naersk {};
      ir-client = naersk.buildPackage {
        src = ./ir-client;
      };
    in {
      enable = true;
      description = "Use tv remote as an input.";
      unitConfig = {
        Type = "simple";
      };
      serviceConfig = {
        ExecStart = "${ir-client}/bin/ir-client";
      };
      wantedBy = ["multi-user.target"];
    };

    meow.home.dataFile."kodi/addons" = {
      enable = true;
      source = "${kodiPackage}/addons";
      recursive = true;
    };

    meow.home.modules = let
      kodiLauncher = pkgs.writeShellScriptBin "kodi" ''
        ${kodiSettings}/bin/create_kodi_settings
        export HOME=${config.stubbornHomeDirectory} # hide log files
        export KODI_DATA=${kodiHome}
        exec "${kodiExecutable}" --audio-backend=pulseaudio "$@" # using pulseaudio fixes some weird pipewire issues
      '';

      specialWorkspace = "special:tv";

      openInKiosk = bin: url:
        pkgs.writeShellScriptBin "${bin}" ''
          kill -s SIGUSR1 $(pidof waybar)
          ${pkgs.hyprland}/bin/hyprctl dispatch togglespecialworkspace ${specialWorkspace}
          ${pkgs.hyprland}/bin/hyprctl dispatch exec "[fullscreen] ${pkgs.firefox}/bin/firefox -P tv --new-window "${url}" --fullscreen"
          kill -s SIGUSR1 $(pidof waybar)
        '';

      tvScripts = pkgs.stdenv.mkDerivation rec {
        name = "tv_scripts";

        unpackPhase = "true";

        youtubeTv = openInKiosk "youtube_tv" "https://youtube.com/tv";
        areenaTv = openInKiosk "areena_tv" "https://areena.yle.fi";
        firefoxTv = openInKiosk "firefox_tv" "";

        installPhase = ''
          mkdir --parents "$out/bin"

          cp ${youtubeTv}/bin/youtube_tv "$out/bin/youtube_tv"
          cp ${areenaTv}/bin/areena_tv "$out/bin/areena_tv"
          cp ${firefoxTv}/bin/firefox_tv "$out/bin/firefox_tv"
        '';
      };
    in [
      ({config, lib, ...}: {
        home.packages = [kodiLauncher tvScripts];

        wayland.windowManager.hyprland.settings = lib.mkIf (builtins.elem "hyprland" config.workstation.environment) {
          workspace = [
            "${specialWorkspace},rounding:false,border:false,shadow:false,gapsin:0,gapsout:0"
          ];
          windowrulev2 = ["fullscreen,class:(Kodi)"];
          exec-once = [
            "${kodiLauncher}/bin/kodi -fs"
          ];
          bind = [
            "ALT, F4, killactive"
            "SUPER, F12, exec, ${kodiLauncher}/bin/kodi -fs"
          ];
        };

        programs.firefox.profiles."tv" = {
          id = 1;
          extraConfig = ''
            user_pref("browser.fullscreen.autohide", true);
          '';
          extensions = with pkgs.firefox-addons; [
            enhancer-for-youtube
            (pkgs.stdenv.mkDerivation rec {
              name = "youtube_for_tv-0.0.3";

              src = pkgs.fetchurl {
                url = "https://addons.mozilla.org/firefox/downloads/file/3420768/youtube_for_tv-0.0.3.xpi";
                hash = "sha256-Xfa7cB4D0Iyfex5y9/jRR93gUkziaIyjqMT0LIOhT6o=";
              };

              addonId = "{d2bcedce-889b-4d53-8ce9-493d8f78612a}";

              buildCommand = ''
                dst="$out/share/mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
                mkdir -p "$dst"
                install -v -m644 "$src" "$dst/${addonId}.xpi"
              '';
            })
          ];
        };

        age.secrets.kodi_youtube_api_keys.path = "${config.xdg.dataHome}/kodi/userdata/addon_data/plugin.video.youtube/api_keys.json";
      })
    ];

    # systemd.services."ir-client" = let
    #   naersk = pkgs.callPackage inputs.naersk {};
    #   ir-client = naersk.buildPackage {
    #     src = ./ir-client;
    #   };
    # in {
    #   enable = true;
    #   description = "Use tv remote as an input.";
    #   unitConfig = {
    #     Type = "simple";
    #   };
    #   serviceConfig = {
    #     ExecStart = "${ir-client}/bin/ir-client";
    #   };
    #   wantedBy = ["multi-user.target"];
    # };
  });
}
