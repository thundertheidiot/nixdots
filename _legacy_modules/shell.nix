# This is not a nix-shell file
{
  config,
  mlib,
  lib,
  pkgs,
  ...
}: let
  cfg = config.meow.shell;

  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;
in {
  options = {
    meow.shell = {
      enable = mkEnOpt "Configure shell.";
    };
  };

  config = mkIf cfg.enable {
    environment.shellAliases = {
      "e" = "setsid -f emacsclient -c";
    };

    # stuff on nixos often requires additional per-project configuration
    # direnv makes that easier, just automatically use shell.nix and flake.nix files
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

    programs.bash = {
      interactiveShellInit = ''
        if [[ $(${pkgs.procps}/bin/ps --no-header --pid=$PPID --format=comm) != "fish" && -z ''${BASH_EXECUTION_STRING} && $TERM != "dumb" ]]
        then
          shopt -q login_shell && LOGIN_OPTION='--login' || LOGIN_OPTION=""
          exec ${pkgs.fish}/bin/fish $LOGIN_OPTION
        fi
      '';
    };

    programs.fish = {
      enable = true;
      interactiveShellInit = ''
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
      '';
    };
  };
}
