{
  mlib,
  lib,
  config,
  pkgs,
  ...
}:
let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.displaylink;
in
{
  options.meow.displaylink = mkEnOpt "DisplayLink support";

  config = mkIf cfg {
    environment.systemPackages = [
      pkgs.displaylink
    ];

    services.xserver.videoDrivers = [
      "displaylink"
      "modesetting"
    ];

    boot = {
      extraModulePackages = [
        (config.boot.kernelPackages.evdi.overrideAttrs (old: {
          postInstall = (old.postInstall or "") + ''
            install -D /dev/null $out/lib/modules/${config.boot.kernelPackages.kernel.modDirVersion}/kernel/drivers/gpu/drm/evdi/evdi.ko.cer
          '';
        }))
      ];

      initrd.kernelModules = [ "evdi" ];
    };
  };
}
