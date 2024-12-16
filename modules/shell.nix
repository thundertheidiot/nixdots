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

    # stuff on nixos often requires additional per-project configuration
    # direnv makes that easier, just automatically use shell.nix and flake.nix files
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    environment.variables = let
      xdgConfig = "$HOME/.config/";
      xdgData = "$HOME/.local/share/";
      xdgCache = "$HOME/.cache/";
      xdgState = "$HOME/.local/state/";
    in {
      SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/keyring/ssh";

      XDG_CONFIG_HOME = "${xdgConfig}";
      XDG_DATA_HOME = "${xdgData}";
      XDG_CACHE_HOME = "${xdgCache}";
      XDG_STATE_HOME = "${xdgState}";

      STUBBORN_HOME = "${config.stubbornHomeDirectory}";
      PATH = "$PATH:$CARGO_HOME/bin";

      # TODO figure out how to move this shit into cleanup.nix, lib.mkAfter or lib.mkBefore don't work
      XNOTIFY_FIFO = "${xdgCache}/xnotify.fifo";
      GNUPGHOME = "${xdgData}/gnupg";
      DVDCSS_CACHE = "${xdgData}/dvdcss";
      LESSHISTFILE = "-";
      XINITRC = "${xdgConfig}/x11/xinitrc";
      ANDROID_SDK_ROOT = "${xdgData}/android";
      CUDA_CACHE_PATH = "${xdgCache}/nv";
      NOTMUCH_CONFIG = "${xdgConfig}/notmuch-config";
      DOCKER_CONFIG = "${xdgConfig}/docker";
      WGETRC = "${xdgConfig}/wget/wgetrc";
      INPUTRC = "${xdgConfig}/shell/inputrc";
      ZDOTDIR = "${xdgConfig}/zsh";
      MACHINE_STORAGE_PATH = "${xdgData}/docker-machine";
      "_JAVA_OPTIONS" = "-Djava.util.prefs.userRoot=${xdgConfig}/java -Djavafx.cachedir=${xdgCache}/openjfx";
      LESSKEY = "${xdgConfig}/less/lesskey";
      ICEAUTHORITY = "${xdgCache}/ICEauthority";
      NPM_CONFIG_USERCONFIG = "${xdgConfig}/npm/npmrc";
      WINEPREFIX = "${xdgData}/wineprefixes/default";
      PASSWORD_STORE_DIR = "${xdgData}/password-store";
      ANDROID_SDK_HOME = "${xdgConfig}/android";
      CARGO_HOME = "${xdgData}/cargo";
      GOPATH = "${xdgData}/go";
      ANSIBLE_CONFIG = "${xdgConfig}/ansible/ansible.cfg";
      UNISON = "${xdgData}/unison";
      HISTFILE = "${xdgData}/history";
      WEECHAT_HOME = "${xdgConfig}/weechat";
      MBSYNCRC = "${xdgConfig}/mbsync/config";
      ELECTRUMDIR = "${xdgData}/electrum";
      XRESOURCES = "${xdgConfig}/x11/xresources";
      NODE_REPL_HISTORY = "${xdgData}/node_repl_history";
      RUSTUP_HOME = "${xdgData}/rustup";
      CGDB_DIR = "${xdgConfig}/cgdb";
      NUGET_PACKAGES = "${xdgCache}/NuGetPackages";
      KDEHOME = "${xdgConfig}/kde";
      PYTHONPYCACHEPREFIX = "${xdgCache}/python";
      PYTHONUSERBASE = "${xdgData}/python";
      GRADLE_USER_HOME = "${xdgData}/gradle";
      RANDFILE = "${xdgData}/openssl_rnd";
      # TERMINFO = "${xdgData}/terminfo";
      # TERMINFO_DIRS = "${xdgData}/terminfo:/usr/share/terminfo";
      ANDROID_HOME = "${xdgData}/android";
      USERXSESSION = "${xdgCache}/X11/xsession";
      USERXSESSIONRC = "${xdgCache}/X11/xsessionrc";
      ALTUSERXSESSION = "${xdgCache}/X11/Xsession";
      ERRFILE = "${xdgCache}/X11/xsession-errors";
      MINETEST_USER_PATH = "${xdgData}/minetest";
      MPLAYER_HOME = "${xdgConfig}/mplayer";
      PYTHONSTARTUP = "${xdgConfig}/pythonrc";
      KERAS_HOME = "${xdgConfig}/keras";
      SQLITE_HISTORY = "${xdgCache}/sqlite_history";
      CONDARC = "${xdgConfig}/conda/condarc";
      XMONAD_CACHE_DIR = "${xdgConfig}/xmonad";
      XMONAD_CONFIG_DIR = "${xdgConfig}/xmonad";
      XMONAD_DATA_DIR = "${xdgConfig}/xmonad";
      QMK_HOME = "${config.meow.home.directory}/.local/src/qmk";

      # fix nixpkgs janet
      JANET_PATH = "${xdgData}/janet/jpm/lib";
      JANET_TREE = "${xdgData}/janet/jpm";
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
            if test -n "$IN_NIX_SHELL"
              echo -n "<nix-shell> "
            end

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
