{
  config,
  lib,
  pkgs,
  localconfig,
  inputs,
  ...
}: let
  customKodi = inputs.custom-kodi.defaultPackage.${localconfig.system};
in {
  config = lib.mkIf (localconfig.install.tv) (with config; {
    xdg.dataFile."kodi/addons" = {
      enable = true;
      recursive = true;
      source = "${customKodi}/share/kodi/addons/";
    };

    home.packages = [
      customKodi
    ];

    wayland.windowManager.hyprland.settings = lib.mkIf (localconfig.install.hyprland) {
      windowrulev2 = [ "fullscreen,class:(Kodi)" ];
      exec-once = [ "${customKodi}/bin/kodi -fs" ];
    };

    programs.firefox.profiles."tv" = {
      id = 1;
      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          ublock-origin
          purpleadblock
          istilldontcareaboutcookies
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
        "plugin.video.invidious" = {
          auto_instance = "false";
          instance_url = "http://127.0.0.1:3000";
          disable_dash = "true";
        };
      };

      # package = pkgs.kodi (pkgs:
      #   with pkgs; [
      #     youtube
      #     netflix
      #     jellyfin
      #     # invidious # maybe good later, not needed right now
      #   ]);
      package = customKodi;
    };
  });
}
