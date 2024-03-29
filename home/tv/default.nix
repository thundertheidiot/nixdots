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

    age.secrets.kodi_youtube_api_keys.path = "${xdg.dataHome}/kodi/userdata/addon_data/plugin.video.youtube/api_keys.json";
    # age.secrets.kodi_jellyfin_data.path = "${xdg.dataHome}/kodi/userdata/addon_data/plugin.video.jellyfin/data.json";

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
