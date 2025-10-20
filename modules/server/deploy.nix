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
    # users.groups.deploy = {};
    users.users.deploy = {
      # group = "deploy";
      group = "wheel";
      isSystemUser = true;

      home = "/tmp/deploy-home";
      createHome = true;

      openssh.authorizedKeys.keys = [cfg.pubkey];

      shell =
        pkgs.writeShellApplication {
          name = "deploy";
          text = ''
            exec /run/wrappers/bin/sudo nixos-rebuild switch --accept-flake-config --verbose --no-reexec --flake github:thundertheidiot/nixdots#${config.networking.hostName}
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
              command = getExe' config.system.build.nixos-rebuild "nixos-rebuild";
              options = ["NOPASSWD"];
            }
            {
              command = "/run/current-system/sw/bin/nixos-rebuild";
              options = ["NOPASSWD"];
            }
          ];
        }
      ];
    };

    # nix.settings.trusted-users = ["deploy"];

    # security.polkit.extraConfig = ''
    #   polkit.addRule(function(action, subject) {
    #     if (subject.user == "deploy") {
    #       if (action.id == "org.nixos.nixos.rebuild-switch" || action.id.indexOf("org.freedesktop.systemd1") === 0) {
    #         return polkit.Result.YES;
    #       }
    #     }
    #   });
    # '';

    # allow deploy user to symlink here
    # systemd.tmpfiles.rules = [
    #   "z /nix/var/nix/profiles 0775 root deploy -"
    #   "z /nix/var/nix/profiles/system 0775 root deploy -"
    # ];
  };
}
