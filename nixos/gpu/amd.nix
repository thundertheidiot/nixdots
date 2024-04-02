{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf (config.setup.gpu == "amd") {
    hardware.opengl.enable = true;
    hardware.opengl.driSupport = true;
    hardware.opengl.driSupport32Bit = config.setup.gaming.enable;

    hardware.opengl.extraPackages = with pkgs; [
      rocmPackages.clr.icd
    ];

    hardware.enableRedistributableFirmware = lib.mkForce true;

    systemd.tmpfiles.rules = [
      "L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"
    ];

    boot.initrd.kernelModules = ["amdgpu"];
    services.xserver.videoDrivers = ["amdgpu"];
  };
}
