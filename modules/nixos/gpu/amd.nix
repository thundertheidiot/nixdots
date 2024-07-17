{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf (config.setup.gpu == "amd") {
    hardware.graphics.enable = true;
    # hardware.opengl.driSupport = true;
    hardware.graphics.enable32Bit = true;

    hardware.graphics.extraPackages = with pkgs; [
      rocmPackages.clr.icd
      vulkan-loader
      vulkan-validation-layers
      vulkan-extension-layer

      # amdvlk
    ];

    hardware.graphics.extraPackages32 = with pkgs; [
      # driversi686Linux.amdvlk
    ];

    environment.systemPackages = with pkgs; [
      lact
    ];

    systemd.services.lactd = {
      enable = true;
      description = "Lact daemon";
      wantedBy = ["multi-user.target"];
      after = ["multi-user.target"];
      serviceConfig = {
        ExecStart = "${pkgs.lact}/bin/lact daemon";
        Nice = -10;
      };
    };

    hardware.enableRedistributableFirmware = lib.mkForce true;

    systemd.tmpfiles.rules = [
      "L+ /opt/rocm/hip - - - - ${pkgs.rocmPackages.clr}"
    ];

    services.xserver.enable = true;
    services.xserver.videoDrivers = ["modesetting"];
    # services.xserver.videoDrivers = ["amdgpu"];
    boot.initrd.kernelModules = ["amdgpu"];
  };
}
