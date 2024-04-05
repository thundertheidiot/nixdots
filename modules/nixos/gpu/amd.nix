{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf (config.setup.gpu == "amd") {
    hardware.opengl.enable = true;
    hardware.opengl.driSupport = true;
    hardware.opengl.driSupport32Bit = true;

    hardware.opengl.extraPackages = with pkgs; [
      rocmPackages.clr.icd
    ];

    hardware.enableRedistributableFirmware = lib.mkForce true;

    systemd.tmpfiles.rules = [
      "L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"
    ];

    services.xserver.enable = true;
    services.xserver.videoDrivers = ["modesetting" "amdgpu"];
    # services.xserver.videoDrivers = ["amdgpu"];
    boot.initrd.kernelModules = ["amdgpu"];
  };
}
