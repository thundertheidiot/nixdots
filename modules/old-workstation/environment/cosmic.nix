# TODO: move
{
  system = {
    lib,
    config,
    pkgs,
    ...
  }:
    lib.mkIf (builtins.elem "cosmic" config.workstation.environment) {
      services.desktopManager.cosmic.enable = true;
    };

  home = {
    lib,
    config,
    pkgs,
    ...
  }:
    lib.mkIf (builtins.elem "cosmic" config.workstation.environment) {
    };
}
