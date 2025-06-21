{
  config,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkEnOptTrue mkOpt;
  inherit (lib.types) str;
in {
  options = {
    mHome.cleanup = mkEnOptTrue "Configuration to clean up $HOME";
    mHome.stubbornHomeDirectory = mkOpt str "${config.home.homeDirectory}/.local/state/home" {};
  };

  config = {
    xdg.configFile = {
      "wget/wgetrc".text = "hsts-file = \"$XDG_CACHE_HOME\"/wget-hsts";
      "npm/npmrc".text = ''
        prefix=${config.xdg.dataHome}/npm
        cache=${config.xdg.cacheHome}/npm
        init-module=${config.xdg.configHome}/npm/config/npm-init.js
      '';
      "python/pythonrc".text = "";
    };

    home.sessionVariables = let
      x = config.xdg;
    in {
      ANDROID_HOME = "${x.configHome}/android";
      ANDROID_SDK_ROOT = "${x.dataHome}/android";
      ANDROID_SDK_HOME = "${x.configHome}/android";
      CUDA_CACHE_PATH = "${x.cacheHome}/nv";
      SQLITE_HISTORY = "${x.cacheHome}/sqlite_history";
      CONDARC = "${x.configHome}/conda/condarc";
      WGETRC = "${x.configHome}/wget/wgetrc";
      QMK_HOME = "${config.home.homeDirectory}/Documents/qmk";
      LESSHISTFILE = "-";
      NPM_CONFIG_USERCONFIG = "${x.configHome}/npm/npmrc";
      WINEPREFIX = "${x.dataHome}/wineprefixes/default";
      LESSKEY = "${x.configHome}/less/lesskey";
      ICEAUTHORITY = "${x.cacheHome}/ICEauthority";
      DVDCSS_CACHE = "${x.dataHome}/dvdcss";
      DOCKER_CONFIG = "${x.configHome}/docker";
      MACHINE_STORAGE_PATH = "${x.dataHome}/docker-machine";
      "_JAVA_OPTIONS" = "-Djava.util.prefs.userRoot=${x.configHome}/java -Djavafx.cachedir=${x.cacheHome}/openjfx";
      RUSTUP_HOME = "${x.dataHome}/rustup";
      KDEHOME = "${x.configHome}/kde";
      PYTHONPYCACHEPREFIX = "${x.cacheHome}/python";
      PYTHONUSERBASE = "${x.dataHome}/python";
      PYTHONSTARTUP = "${x.configHome}/pythonrc";
      CARGO_HOME = "${x.dataHome}/cargo";
      GOPATH = "${x.dataHome}/go";
    };

    xdg.userDirs = {
      desktop = "${config.home.homeDirectory}/.local/share/xdg-dirs/desktop";
      publicShare = "${config.home.homeDirectory}/.local/share/xdg-dirs/publicshare";
      templates = "${config.home.homeDirectory}/.local/share/xdg-dirs/templates";
    };

    home.activation.createStubbornHome = ''
      mkdir --parents ${config.mHome.stubbornHomeDirectory}
    '';
  };
}
