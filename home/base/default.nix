{
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}: let
  environment = {
    XNOTIFY_FIFO = "$XDG_CACHE_HOME/xnotify.fifo";
    GNUPGHOME = "$XDG_DATA_HOME/gnupg";
    DVDCSS_CACHE = "$XDG_DATA_HOME/dvdcss";
    #HISTFILE = "$HOME/.config/zsh/histfile";
    XINITRC = "$XDG_CONFIG_HOME/x11/xinitrc";
    ANDROID_SDK_ROOT = "$XDG_DATA_HOME/android";
    CUDA_CACHE_PATH = "$XDG_CACHE_HOME/nv";
    NOTMUCH_CONFIG = "$XDG_CONFIG_HOME/notmuch-config";
    DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    WGETRC = "$XDG_CONFIG_HOME/wget/wgetrc";
    INPUTRC = "$XDG_CONFIG_HOME/shell/inputrc";
    ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
    MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker-machine";
    "_JAVA_OPTIONS" = "-Djava.util.prefs.userRoot=$XDG_CONFIG_HOME/java -Djavafx.cachedir=$XDG_CACHE_HOME/openjfx";
    LESSKEY = "$XDG_CONFIG_HOME/less/lesskey";
    LESSHISTFILE = "-";
    ICEAUTHORITY = "$XDG_CACHE_HOME/ICEauthority";
    NPM_CONFIG_USERCONFIG = "$XDG_CONFIG_HOME/npm/npmrc";
    WINEPREFIX = "$XDG_DATA_HOME/wineprefixes/default";
    KODI_DATA = "$XDG_DATA_HOME/kodi";
    PASSWORD_STORE_DIR = "$XDG_DATA_HOME/password-store";
    TMUX_TMPDIR = "$XDG_RUNTIME_DIR";
    ANDROID_SDK_HOME = "$XDG_CONFIG_HOME/android";
    CARGO_HOME = "$XDG_DATA_HOME/cargo";
    GOPATH = "$XDG_DATA_HOME/go";
    ANSIBLE_CONFIG = "$XDG_CONFIG_HOME/ansible/ansible.cfg";
    UNISON = "$XDG_DATA_HOME/unison";
    HISTFILE = "$XDG_DATA_HOME/history";
    WEECHAT_HOME = "$XDG_CONFIG_HOME/weechat";
    MBSYNCRC = "$HOME/.config/mbsync/config";
    ELECTRUMDIR = "$XDG_DATA_HOME/electrum";
    XRESOURCES = "$XDG_CONFIG_HOME/x11/xresources";
    SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/gcr/ssh";
    NODE_REPL_HISTORY = "$XDG_DATA_HOME/node_repl_history";
    RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
    CGDB_DIR = "$XDG_CONFIG_HOME/cgdb";
    NUGET_PACKAGES = "$XDG_CACHE_HOME/NuGetPackages";
    KDEHOME = "$XDG_CONFIG_HOME/kde";
    PYTHONPYCACHEPREFIX = "$XDG_CACHE_HOME/python";
    PYTHONUSERBASE = "$XDG_DATA_HOME/python";
    GRADLE_USER_HOME = "$XDG_DATA_HOME/gradle";
    RANDFILE = "$XDG_DATA_HOME/openssl_rnd";
    # TERMINFO = "$XDG_DATA_HOME/terminfo";
    # TERMINFO_DIRS = "$XDG_DATA_HOME/terminfo:/usr/share/terminfo";
    ANDROID_HOME = "$XDG_DATA_HOME/android";
    USERXSESSION = "$XDG_CACHE_HOME/X11/xsession";
    USERXSESSIONRC = "$XDG_CACHE_HOME/X11/xsessionrc";
    ALTUSERXSESSION = "$XDG_CACHE_HOME/X11/Xsession";
    ERRFILE = "$XDG_CACHE_HOME/X11/xsession-errors";
    MINETEST_USER_PATH = "$XDG_DATA_HOME/minetest";
    MPLAYER_HOME = "$XDG_CONFIG_HOME/mplayer";
    PYTHONSTARTUP = "$XDG_CONFIG_HOME/pythonrc";
    KERAS_HOME = "$XDG_CONFIG_HOME/keras";
    SQLITE_HISTORY = "$XDG_CACHE_HOME/sqlite_history";
    CONDARC = "$XDG_CONFIG_HOME/conda/condarc";
    XMONAD_CACHE_DIR = "$XDG_CONFIG_HOME/xmonad";
    XMONAD_CONFIG_DIR = "$XDG_CONFIG_HOME/xmonad";
    XMONAD_DATA_DIR = "$XDG_CONFIG_HOME/xmonad";
    MANGOHUD_CONFIGFILE = "$XDG_CONFIG_HOME/mangohud.conf";

    # Settings

    MOZ_USE_XINPUT2 = "1";
    NVIM_LISTEN_ADDRESS = "/tmp/nvimsocket";
  };
in
  with config; {
    home.sessionVariables = environment;
    systemd.user.sessionVariables = environment;

    imports = [
      ./scripts.nix
    ];

    home.packages = with pkgs; [
      fd
      ripgrep
      ncdu
      btop
      jq
      killall
      nvtopPackages.full
      pulsemixer
      alejandra
    ];

    xdg.enable = true;

    home.file.".config/wget/wgetrc" = {
      text = "hsts-file = \"$XDG_CACHE_HOME\"/wget-hsts";
    };

    home.file.".config/npm/npmrc".text = ''
      prefix=${xdg.dataHome}/npm
      cache=${xdg.cacheHome}/npm
      init-module=${xdg.configHome}/npm/config/npm-init.js
    '';

    programs.bash = {
      enable = true;
      profileExtra = ''
        export PATH="$PATH:$HOME/.local/bin:$XDG_DATA_HOME/cargo/bin"
      '';
      initExtra = ''
        if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
        then
          shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
          exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
        fi
      '';
    };

    programs.fish = {
      enable = true;
      functions = {
        fish_prompt = ''
          echo (set_color purple)$USER(set_color normal)'@'(set_color blue)(uname -n)(set_color normal) (pwd) '> '
        '';
      };
      shellAliases = {
        "m" = "mpv --no-video --loop=yes";
        "e" = "setsid -f emacsclient -c";
      };
    };
  }
