{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf (config.setup.gpu == "intel") {
    hardware.graphics.enable = true;
    hardware.graphics.enable32Bit = config.setup.gaming.enable;

    boot.initrd.kernelModules = ["i915"];

    hardware.graphics.extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      libvdpau-va-gl
    ];
  };
}
