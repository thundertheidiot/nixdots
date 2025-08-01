{lib, ...}: let
  inherit (lib) mkForce;
in {
  flake.modules.nixos.autoupgrade = {
    system.autoUpgrade = {
      enable = true;
      flake = "github:thundertheidiot/nixdots";
      operation = "boot";
      persistent = true;
      dates = "daily";
    };
  };

  flake.modules.nixos."autoupgrade/server" = {
    config,
    pkgs,
    ...
  }: {
    # This one reboots afterwards
    system.autoUpgrade = {
      enable = true;

      flake = "github:thundertheidiot/nixdots";
      persistent = true;

      dates = "02:00";
    };

    systemd.services.nixos-upgrade.script = let
      cfg = config.system.autoUpgrade;
      nixos-rebuild = "${config.system.build.nixos-rebuild}/bin/nixos-rebuild";
      readlink = "${pkgs.coreutils}/bin/readlink";
      shutdown = "${config.systemd.package}/bin/shutdown";
      upgradeFlag = lib.optional (cfg.channel == null) "--upgrade";
    in (mkForce ''
      ${nixos-rebuild} boot ${toString (cfg.flags ++ upgradeFlag)}
      booted="$(${readlink} /run/booted-system/)"
      built="$(${readlink} /nix/var/nix/profiles/$(${readlink}/nix/var/nix/profiles/system))"

      if [ "''${booted}" = "''${built}" ]; then
        echo "Configuration not changed, no need to reboot"
      else
        ${shutdown} -r +1
      fi
    '');
  };
}
