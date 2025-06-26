{
  config,
  lib,
  pkgs,
  mlib,
  ...
}: let
  cfg = config.meow.gpu;

  inherit (lib) mkMerge mkIf;

  inherit (lib.types) enum;
  inherit (mlib) mkOpt;
in {
  options = {
    meow.gpu = mkOpt (enum ["amd" "nvidia" "intel" "none"]) "none" {
      description = "Gpu type, this is used for installing drivers.";
    };
  };

  config = mkMerge [
    {
      hardware.graphics.enable = true;
      hardware.graphics.enable32Bit = config.meow.gaming.enable;
    }
    (mkIf (cfg == "intel") {
      boot.initrd.kernelModules = ["i915"];

      environment.systemPackages = [pkgs.nvtopPackages.intel];

      hardware.graphics.extraPackages = with pkgs; [
        intel-media-driver
        intel-vaapi-driver
        libvdpau-va-gl
      ];
    })
    (mkIf (cfg == "amd") {
      hardware.graphics.extraPackages = with pkgs; [
        rocmPackages.clr.icd
        vulkan-loader
        vulkan-validation-layers
        vulkan-extension-layer
      ];

      environment.systemPackages = with pkgs; [
        lact
        nvtopPackages.amd
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
      boot.initrd.kernelModules = ["amdgpu"];
    })
  ];
}
