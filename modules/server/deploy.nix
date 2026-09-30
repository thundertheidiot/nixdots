{
  config,
  lib,
  mlib,
  pkgs,
  ...
}:
let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib.types) nullOr str;
  inherit (lib) mkIf getExe';

  cfg = config.meow.server.deploy;
in
{
  options.meow.server.deploy = {
    enable = mkEnOpt "Enable deploy user for remote upgrades.";
    pubkey = mkOpt (nullOr str) null {
      description = "SSH Key to allow authorization from.";
    };
  };

  config = mkIf cfg.enable (
    let
      # point to real path in nix store
      nixos-rebuild = getExe' config.system.build.nixos-rebuild "nixos-rebuild";
    in
    {
      users.groups.deploy = { };
      users.users.deploy = {
        group = "deploy";
        isSystemUser = true;

        home = "/tmp/deploy-home";
        createHome = true;

        openssh.authorizedKeys.keys = [ cfg.pubkey ];

        shell =
          pkgs.writeShellApplication {
            name = "deploy";
            text = ''
              # sshd invokes the login shell with -c and the requested revision.
              if [[ $# != 2 || "$1" != -c || ! "$2" =~ ^[0-9a-f]{40}$ ]]; then
                echo "Expected a full Git commit revision as the SSH command" >&2
                exit 1
              fi
              exec /run/wrappers/bin/sudo ${nixos-rebuild} switch --accept-flake-config --no-reexec --flake "github:thundertheidiot/nixdots/$2#${config.networking.hostName}"
            '';
          }
          + "/bin/deploy";
      };

      security.sudo-rs = {
        extraRules = [
          {
            users = [ "deploy" ];
            commands = [
              {
                command = nixos-rebuild;
                options = [ "NOPASSWD" ];
              }
            ];
          }
        ];
      };
    }
  );
}
