# This is not a nix-shell file
{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  cfg = config.meow.shell;

  inherit (mlib) mkOpt mkEnOpt homeModule;
  inherit (lib) mkIf;
  inherit (lib.types) str;
in {
  options = {
    meow.shell = {
      enable = mkEnOpt "Configure shell.";
      prompt = mkOpt str "echo (set_color purple)$USER(set_color normal)'@'(set_color blue)(uname -n)(set_color normal) (pwd) '> '" {
        description = "Prompt for fish.";
      };
    };
  };

  config = {
    environment.shellAliases = {
      "e" = "setsid -f emacsclient -c";
    };

    environment.variables = {
      SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/keyring/ssh";

      XDG_CONFIG_HOME = "$HOME/.config/";
      XDG_DATA_HOME = "$HOME/.local/share/";
      XDG_CACHE_HOME = "$HOME/.cache/";
      XDG_STATE_HOME = "$HOME/.local/state/";

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
      PASSWORD_STORE_DIR = "$XDG_DATA_HOME/password-store";
      ANDROID_SDK_HOME = "$XDG_CONFIG_HOME/android";
      CARGO_HOME = "$XDG_DATA_HOME/cargo";
      GOPATH = "$XDG_DATA_HOME/go";
      ANSIBLE_CONFIG = "$XDG_CONFIG_HOME/ansible/ansible.cfg";
      UNISON = "$XDG_DATA_HOME/unison";
      HISTFILE = "$XDG_DATA_HOME/history";
      WEECHAT_HOME = "$XDG_CONFIG_HOME/weechat";
      MBSYNCRC = "$XDG_CONFIG_HOME/mbsync/config";
      ELECTRUMDIR = "$XDG_DATA_HOME/electrum";
      XRESOURCES = "$XDG_CONFIG_HOME/x11/xresources";
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

      PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";

      QMK_HOME = "${config.homeDirectory}/.local/src/qmk";

      STUBBORN_HOME = "${config.stubbornHomeDirectory}";

      PATH = "$PATH:$CARGO_HOME/bin";
    };

    programs.bash = {
      interactiveShellInit = ''
        if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} ]]
        then
          shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
          exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
        fi
      '';
    };

    programs.fish = {
      enable = true;
      shellAliases =
        config.environment.shellAliases
        // {
          "m" = "mpv --no-video --loop=yes";
        };
      interactiveShellInit =
        ''
          function fish_prompt
            ${cfg.prompt}
          end

          # Keep shell when entering nix-shell or nix run
          # ${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source
        ''
        + (
          if config.meow.emacs.enable
          then ''
             function vterm_printf;
                if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
                    # tell tmux to pass the escape sequences through
                    printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
                else if string match -q -- "screen*" "$TERM"
                    # GNU screen (screen, screen-256color, screen-256color-bce)
                    printf "\eP\e]%s\007\e\\" "$argv"
                else
                    printf "\e]%s\e\\" "$argv"
                end
            end
          ''
          else ""
        );
    };
  };
}
