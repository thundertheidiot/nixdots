{
  config,
  lib,
  pkgs,
  localconfig,
  inputs,
  ...
}: let
  addonDir = "/share/kodi/addons";
  yleNamespace = "plugin.video.yleareena.jade";

  yleareena = pkgs.stdenv.mkDerivation rec {
    name = "kodi-yleareena-1.3.1";
    dontStrip = true;

    extraRuntimeDependencies = [];
    propagatedBuildInputs = [];

    src = pkgs.fetchgit {
      url = "https://github.com/aajanki/${yleNamespace}";
      rev = "9b89fabf6ad1cae08d92b3309029061ca9ab66e5";
      hash = "sha256-uLWWJTX6aHsjzJneqab2VczJO+33tg8nP7fPQ/GpqZg=";
    };

    passthru = {
      pythonPath = "resources/lib";
    };

    installPhase = ''
      runHook preInstall

      cd ./$sourceDir
      d=$out${addonDir}/${yleNamespace}
      mkdir -p $d
      sauce="."
      [ -d ${yleNamespace} ] && sauce=${yleNamespace}
      cp -R "$sauce/"* $d

      runHook postInstall
    '';
  };
in {
  config = lib.mkIf (localconfig.install.tv) (with config; {
    home.packages = [
      yleareena
    ];

    xdg.dataFile."kodi/addons/${yleNamespace}" = {
      enable = true;
      recursive = true;
      source = "${yleareena}/share/kodi/addons/${yleNamespace}";
    };

    age.secrets.youtube_api_keys.path = "${xdg.dataHome}/kodi/userdata/addon_data/plugin.video.youtube/api_keys.json";

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

      package = pkgs.kodi.withPackages (pkgs:
        with pkgs; [
          youtube
          netflix
          jellyfin
          invidious
        ]);
    };
  });
}
