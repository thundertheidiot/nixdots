{
  config,
  lib,
  pkgs,
  mlib,
  inputs,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf mkMerge;

  cfg = config.meow.comin;
in
{
  options.meow = {
    comin.enable = mkEnOpt "Enable comin deployment";
  };

  config = mkMerge [
    {
      # read below, local switches are stamped too
      system.configurationRevision = inputs.self.rev or inputs.self.dirtyRev or null;

      # warns when the system running on a host isn't in the local checkout's HEAD
      environment.systemPackages = [
        (pkgs.writeShellApplication {
          name = "nixdots-behind";
          runtimeInputs = [
            pkgs.git
            pkgs.openssh
          ];
          text = ''
            host=''${1:-$(uname -n)}
            if [[ "$host" == "$(uname -n)" ]]; then
              rev=$(/run/current-system/sw/bin/nixos-version --configuration-revision 2>/dev/null) || exit 0
            else
              rev=$(ssh "$host" nixos-version --configuration-revision 2>/dev/null) || exit 0
            fi
            rev=''${rev%-dirty}
            [[ -n "$rev" ]] || exit 0

            if ! git cat-file -e "$rev^{commit}" 2>/dev/null; then
              echo "$host is running ''${rev:0:7}, which isn't in this repo, pull first"
              exit 1
            fi
            if ! git merge-base --is-ancestor "$rev" HEAD; then
              echo "$host is running ''${rev:0:7}, which isn't in HEAD, pull first"
              exit 1
            fi
          '';
        })
      ];

      programs.fish.interactiveShellInit = ''
        function __nixdots_behind --on-variable PWD
          set -l root (git rev-parse --show-toplevel 2>/dev/null); or return
          test -f $root/flake/mksystem.nix; and command -q nixdots-behind; or return
          set -l msg (nixdots-behind); or echo (set_color red)$msg(set_color normal)
        end
        __nixdots_behind
      '';
    }

    (mkIf cfg.enable {
      meow.impermanence.directories = [ "/var/lib/comin" ];

      services.comin = {
        enable = true;
        remotes = [
          {
            name = "origin";
            url = "https://github.com/thundertheidiot/nixdots.git";
            branches.main.name = "main";
          }
        ];
      };
    })
  ];
}
