{
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}: let
  xdg = config.xdg;
  environment = {
    XNOTIFY_FIFO = "${xdg.cacheHome}/xnotify.fifo";
    GNUPGHOME = "${xdg.dataHome}/gnupg";
    DVDCSS_CACHE = "${xdg.dataHome}/dvdcss";
    #HISTFILE = "$HOME/.config/zsh/histfile";
    XINITRC = "${xdg.configHome}/x11/xinitrc";
    ANDROID_SDK_ROOT = "${xdg.dataHome}/android";
    CUDA_CACHE_PATH = "${xdg.cacheHome}/nv";
    NOTMUCH_CONFIG = "${xdg.configHome}/notmuch-config";
    DOCKER_CONFIG = "${xdg.configHome}/docker";
    WGETRC = "${xdg.configHome}/wget/wgetrc";
    INPUTRC = "${xdg.configHome}/shell/inputrc";
    ZDOTDIR = "${xdg.configHome}/zsh";
    MACHINE_STORAGE_PATH = "${xdg.dataHome}/docker-machine";
    "_JAVA_OPTIONS" = "-Djava.util.prefs.userRoot=${xdg.configHome}/java -Djavafx.cachedir=${xdg.cacheHome}/openjfx";
    LESSKEY = "${xdg.configHome}/less/lesskey";
    LESSHISTFILE = "-";
    ICEAUTHORITY = "${xdg.cacheHome}/ICEauthority";
    NPM_CONFIG_USERCONFIG = "${xdg.configHome}/npm/npmrc";
    WINEPREFIX = "${xdg.dataHome}/wineprefixes/default";
    PASSWORD_STORE_DIR = "${xdg.dataHome}/password-store";
    ANDROID_SDK_HOME = "${xdg.configHome}/android";
    CARGO_HOME = "${xdg.dataHome}/cargo";
    GOPATH = "${xdg.dataHome}/go";
    ANSIBLE_CONFIG = "${xdg.configHome}/ansible/ansible.cfg";
    UNISON = "${xdg.dataHome}/unison";
    HISTFILE = "${xdg.dataHome}/history";
    WEECHAT_HOME = "${xdg.configHome}/weechat";
    MBSYNCRC = "${xdg.configHome}/mbsync/config";
    ELECTRUMDIR = "${xdg.dataHome}/electrum";
    XRESOURCES = "${xdg.configHome}/x11/xresources";
    SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/gcr/ssh";
    NODE_REPL_HISTORY = "${xdg.dataHome}/node_repl_history";
    RUSTUP_HOME = "${xdg.dataHome}/rustup";
    CGDB_DIR = "${xdg.configHome}/cgdb";
    NUGET_PACKAGES = "${xdg.cacheHome}/NuGetPackages";
    KDEHOME = "${xdg.configHome}/kde";
    PYTHONPYCACHEPREFIX = "${xdg.cacheHome}/python";
    PYTHONUSERBASE = "${xdg.dataHome}/python";
    GRADLE_USER_HOME = "${xdg.dataHome}/gradle";
    RANDFILE = "${xdg.dataHome}/openssl_rnd";
    # TERMINFO = "${xdg.dataHome}/terminfo";
    # TERMINFO_DIRS = "${xdg.dataHome}/terminfo:/usr/share/terminfo";
    ANDROID_HOME = "${xdg.dataHome}/android";
    USERXSESSION = "${xdg.cacheHome}/X11/xsession";
    USERXSESSIONRC = "${xdg.cacheHome}/X11/xsessionrc";
    ALTUSERXSESSION = "${xdg.cacheHome}/X11/Xsession";
    ERRFILE = "${xdg.cacheHome}/X11/xsession-errors";
    MINETEST_USER_PATH = "${xdg.dataHome}/minetest";
    MPLAYER_HOME = "${xdg.configHome}/mplayer";
    PYTHONSTARTUP = "${xdg.configHome}/pythonrc";
    KERAS_HOME = "${xdg.configHome}/keras";
    SQLITE_HISTORY = "${xdg.cacheHome}/sqlite_history";
    CONDARC = "${xdg.configHome}/conda/condarc";
    XMONAD_CACHE_DIR = "${xdg.configHome}/xmonad";
    XMONAD_CONFIG_DIR = "${xdg.configHome}/xmonad";
    XMONAD_DATA_DIR = "${xdg.configHome}/xmonad";
    MANGOHUD_CONFIGFILE = "${xdg.configHome}/mangohud.conf";

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
      rustup
      killall
      nvtopPackages.full
      pulsemixer
      alejandra
      rsync
      agenix.default
      age
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
