{
  config,
  lib,
  ...
}: let
  inherit (builtins) elem;
  inherit (lib) mkIf mkForce;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "gnome" env) {
    services.xserver.desktopManager.gnome.enable = true;
    # services.gnome.gnome-online-accounts.enable = false;
  };
}
