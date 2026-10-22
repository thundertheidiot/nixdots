{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = {
    hardware.framework.enableKmod = false;

    boot = {
      extraModulePackages = with config.boot.kernelPackages; [
        (framework-laptop-kmod.overrideAttrs (old: {
          postInstall = (old.postInstall or "") + ''
            touch $out/lib/modules/${config.boot.kernelPackages.kernel.modDirVersion}/extra/framework_laptop.ko.cer
          '';
        }))
      ];

      # https://github.com/DHowett/framework-laptop-kmod?tab=readme-ov-file#usage
      kernelModules = [
        "cros_ec"
        "cros_ec_lpcs"
      ];
    };
  };
}
