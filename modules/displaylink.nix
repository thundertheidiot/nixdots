{
  mlib,
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf mkForce;

  cfg = config.meow.displaylink;
in {
  options.meow.displaylink = mkEnOpt "DisplayLink support";

  config = mkIf cfg {
    environment.systemPackages = [
      pkgs.displaylink
    ];

    services.xserver.videoDrivers = ["displaylink" "modesetting"];

    boot = {
      extraModulePackages = [
        config.boot.kernelPackages.evdi
      ];

      kernelPackages = mkForce pkgs.linuxKernel.packages.linux_6_17;

      initrd.kernelModules = ["evdi"];
    };
  };
}
