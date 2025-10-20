{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt mkOpt;
  inherit (lib.types) nullOr str;
  inherit (lib) mkIf getExe';

  cfg = config.meow.server.deploy;
in {
  options.meow.server.deploy = {
    enable = mkEnOpt "Enable deploy user for remote upgrades.";
    pubkey = mkOpt (nullOr str) null {
      description = "SSH Key to allow authorization from.";
    };
  };

  config = mkIf cfg.enable (let
    # point to real path in nix store
    nixos-rebuild = getExe' config.system.build.nixos-rebuild "nixos-rebuild";
  in {
    users.groups.deploy = {};
    users.users.deploy = {
      group = "deploy";
      isSystemUser = true;

      home = "/tmp/deploy-home";
      createHome = true;

      openssh.authorizedKeys.keys = [cfg.pubkey];

      shell =
        pkgs.writeShellApplication {
          name = "deploy";
          text = ''
            exec /run/wrappers/bin/sudo ${nixos-rebuild} switch --accept-flake-config --verbose --no-reexec --flake github:thundertheidiot/nixdots#${config.networking.hostName}
          '';
        }
        + "/bin/deploy";
    };

    security.sudo-rs = {
      extraRules = [
        {
          users = ["deploy"];
          commands = [
            {
              command = nixos-rebuild;
              options = ["NOPASSWD"];
            }
          ];
        }
      ];
    };
  });
}
