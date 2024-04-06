{
  config,
  lib,
  pkgs,
  ...
}: let
  customKodi = import ./kodi.nix {inherit config pkgs lib;};

  guisettings = ''
    <settings version="2">
      <setting id="lookandfeel.skin">skin.estuary.modv2</setting>
      <setting id="locale.timezonecountry">Finland</setting>
      <setting id="locale.timezone">Europe/Helsinki</setting>

      <setting id="locale.use24hourclock">true</setting>
    </settings>
  '';

  createSettings = pkgs.writeShellScriptBin "createSettings" ''
    userdata="${config.programs.kodi.datadir}/userdata"
    guisettings="${config.programs.kodi.datadir}/userdata/guisettings.xml"

    [ ! -d "$userdata" ] && mkdir --parents "$userdata"
    [ ! -f "$guisettings" ] && echo "${guisettings}" > "$guisettings"

    [ ! -d  "$userdata/addon_data/script.skinshortcuts" ] && cp -r --dereference "${customKodi}/share/kodi/userdata/addon_data/script.skinshortcuts" "$userdata/addon_data/script.skinshortcuts"
    chmod -R 777 "$userdata/addon_data/script.skinshortcuts"

    addondata="$userdata/addon_data"
    for i in plugin.video.jellyfin plugin.video.netflix plugin.video.youtube script.module.pvr.artwork service.xbmc.versioncheck; do
      mkdir --parents "$i"
      echo "<settings version="2">\n</settings>" >> "$i"/settings.xml
    done
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
in {
  config = lib.mkIf (config.setup.tv.enable) (with config; {
    xdg.dataFile."kodi/addons" = {
      enable = true;
      recursive = true;
      source = "${customKodi}/share/kodi/addons/";
    };

    # xdg.dataFile."kodi/userdata/addon_data/script.skinshortcuts" = {
    #   enable = true;
    #   recursive = true;
    #   source = ./script.skinshortcuts-settings;
    # };

    home.packages = [
      customKodi
      tvScripts
      createSettings
    ];

    wayland.windowManager.hyprland.settings = lib.mkIf (config.setup.hyprland.enable) {
      workspace = [
        "${specialWorkspace},rounding:false,border:false,shadow:false,gapsin:0,gapsout:0"
      ];
      windowrulev2 = [ "fullscreen,class:(Kodi)" ];
      exec-once = [
        "${createSettings}/bin/createSettings && ${customKodi}/bin/kodi_with_addons -fs"
      ];
      bind = ["ALT, F4, killactive"];
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

    age.secrets.kodi_youtube_api_keys.path = "${xdg.dataHome}/kodi/userdata/addon_data/plugin.video.youtube/api_keys.json";

    programs.kodi = {
      enable = true;
      datadir = "${xdg.dataHome}/kodi";

      addonSettings = {
      };

      package = customKodi;
    };
  });
}
