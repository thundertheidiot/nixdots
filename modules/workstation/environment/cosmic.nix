{
  config,
  lib,
  ...
}: let
  inherit (builtins) elem;
  inherit (lib) mkIf;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "cosmic" env) {
    services.desktopManager.cosmic.enable = true;
  };
}
