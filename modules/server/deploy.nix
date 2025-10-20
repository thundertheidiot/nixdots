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

      home = "/tmp/deploy-home";
      createHome = true;

      openssh.authorizedKeys.keys = [cfg.pubkey];

      shell =
        pkgs.writeShellApplication {
          name = "deploy";
          text = ''
            exec nixos-rebuild switch --accept-flake-config --no-reexec --flake github:thundertheidiot/nixdots#${config.networking.hostName}
          '';
        }
        + "/bin/deploy";
    };

    nix.settings.trusted-users = ["deploy"];

    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (subject.user == "deploy" && action.id == "org.nixos.nixos.rebuild-switch") {
          return polkit.Result.YES;
        }
      });
    '';

    # allow deploy user to symlink here
    systemd.tmpfiles.rules = [
      "z /nix/var/nix/profiles 0775 root deploy -"
      "z /nix/var/nix/profiles/system 0775 root deploy -"
    ];
  };
}
