{
  config,
  lib,
  pkgs,
  mlib,
  ...
}: let
  inherit (lib) mkIf mkForce elem;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "plasma" env) {
    services.desktopManager.plasma6 = {
      enable = true;
    };
    services.orca.enable = mkForce false;

    meow.home.modules = [
      {
        mHome.themeQt = false;
        mHome.themeGtk = false;
      }
    ];
  };
}
