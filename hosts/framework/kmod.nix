{
  pkgs,
  lib,
  config,
  ...
}: {
  config = {
    hardware.framework.enableKmod = false;

    boot = {
      extraModulePackages = with config.boot.kernelPackages; [
        framework-laptop-kmod
      ];

      # https://github.com/DHowett/framework-laptop-kmod?tab=readme-ov-file#usage
      kernelModules = [
        "cros_ec"
        "cros_ec_lpcs"
      ];
    };
  };
}
