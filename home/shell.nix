{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOptTrue;
  inherit (lib) mkIf;

  cfg = config.mHome.shell;
in {
  options = {
    mHome.shell.enable = mkEnOptTrue "Configure fish shell.";
  };

  config = mkIf cfg.enable {
    xdg.enable = true;

    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    home.sessionSearchVariables = {
      PATH = [
        "$PATH"
        "$CARGO_HOME/bin"
      ];
    };

    home.sessionVariables = {
      SSH_AUTH_SOCK = "$XDG_RUNTIME_DIR/keyring/ssh";

      STUBBORN_HOME = "${config.mHome.stubbornHomeDirectory}";
    };

    programs.fish = {
      enable = true;
      shellAliases = {
        "e" = "setsid -f emacsclient -c";
        "m" = "mpv --no-video --loop=yes";
      };
      interactiveShellInit =
        ''
          set fish_greeting

          function fish_prompt
            if test -n "$IN_NIX_SHELL"
              echo -n "<nix-shell> "
            end

            if [ "$TERM" = "dumb" ]
              echo $USER'@'(uname -n) (pwd) '> '
            else
              echo (set_color purple)$USER(set_color normal)'@'(set_color blue)(uname -n)(set_color normal) (pwd) '> '
            end
          end
        ''
        + (
          if config.mHome.emacs.enable
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
