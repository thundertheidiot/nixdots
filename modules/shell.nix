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
          ${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source
        ''
        + (if config.meow.emacs.enable then ''
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
        '' else "");
    };
  };
}
