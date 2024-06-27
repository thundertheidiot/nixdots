{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf (config.setup.gpu == "intel") {
    hardware.opengl.enable = true;
    hardware.opengl.driSupport = true;
    hardware.opengl.driSupport32Bit = config.setup.gaming.enable;

    boot.initrd.kernelModules = ["i915"];

    hardware.opengl.extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      libvdpau-va-gl
    ];
  };
}
