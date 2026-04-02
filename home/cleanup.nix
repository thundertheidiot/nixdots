{
  config,
  mlib,
  pkgs,
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

    systemd.user.sessionVariables = removeAttrs config.home.sessionVariables ["EDITOR" "VISUAL"];
    home.sessionVariables = let
      x = config.xdg;
    in {
      GRIPHOME = "${x.configHome}/grip"; # python-grip ~/.grip
      OMNISHARPHOME = "${x.configHome}/omnisharp"; # omnisharp-roslyn ~/.omnisharp
      NUGET_PACKAGES = "${x.cacheHome}/nugetpackages"; # nuget ~/.nuget/packages
      ANDROID_HOME = "${x.configHome}/android";
      ANDROID_SDK_ROOT = "${x.dataHome}/android";
      ANDROID_SDK_HOME = "${x.configHome}/android";
      ADB_VENDOR_KEYS = "${x.stateHome}/adb"; # adb ~/.android
      XCOMPOSECACHE = "${x.cacheHome}/xcompose"; # xcompose ~/.compose-cache

      RENPY_PATH_TO_SAVES = "${x.dataHome}/renpy"; # ~/.renpy
      RENPY_MULTIPERSISTENT = "${x.dataHome}/renpy_shared"; # ~/.renpy

      TEXMFHOME = "${x.dataHome}/texmf"; # ~/.texlive
      TEXMFVAR = "${x.cacheHome}/texlive/texmf-var";
      TEXMFCONFIG = "${x.configHome}/texlive/texmf-config";

      ELECTRUMDIR = "${x.dataHome}/electrum"; # ~/.electrum
      ELECTRUMLTC_DIR = "${x.dataHome}/electrum-ltc"; # ~/.electrum-ltc

      HISTFILE = "${x.dataHome}/bash_history";

      LEIN_HOME = "${x.dataHome}/lein";
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
      PYTHON_HISTORY = "${x.stateHome}/python_history";
      CARGO_HOME = "${x.dataHome}/cargo"; # .cargo
      GOPATH = "${x.dataHome}/go";
      STUBBORN_HOME = "${config.mHome.stubbornHomeDirectory}";
    };

    programs.gpg.homedir = "${config.xdg.dataHome}/gnupg";

    xdg.userDirs = {
      desktop = "${config.home.homeDirectory}/.local/share/xdg-dirs/desktop";
      publicShare = "${config.home.homeDirectory}/.local/share/xdg-dirs/publicshare";
      templates = "${config.home.homeDirectory}/.local/share/xdg-dirs/templates";
    };

    home.preferXdgDirectories = true;
    home.pointerCursor.dotIcons.enable = false;

    gtk.gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";

    home.activation.createStubbornHome = ''
      mkdir --parents ${config.mHome.stubbornHomeDirectory}
    '';

    programs.bash.historyFile = "${config.xdg.dataHome}/bash_history";

    home.packages = [
      (pkgs.writeShellScriptBin "wraphome" ''
        [ ! -z "$STUBBORN_HOME_DIRECTORY" ] && export HOME=$STUBBORN_HOME_DIRECTORY
        exec "$@"
      '')
    ];
  };
}
