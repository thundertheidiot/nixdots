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

  config = mkIf cfg.enable {
    users.groups.deploy = {};
    users.users.deploy = {
      group = "deploy";
      isSystemUser = true;
      openssh.authorizedKeys.keys = [cfg.pubkey];
      shell = pkgs.writeShellApplication {
        name = "deploy";
        runtimeInputs = [pkgs.nh];
        text = ''
          exec sudo nixos-rebuild switch --sudo --accept-flake-config --no-reexec --flake github:thundertheidiot/nixdots#nixosConfigurations.${config.networking.hostName}
        '';
      };
    };

    security.sudo-rs = {
      extraRules = [
        {
          users = ["deploy"];
          commands = [
            {
              command = getExe' config.system.build.nixos-rebuild "nixos-rebuild";
              options = ["NOPASSWD"];
            }
          ];
        }
      ];
    };
  };
}
