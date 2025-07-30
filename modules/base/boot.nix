{lib, ...}: let
  inherit (lib) mkDefault;
in {
  flake.modules.nixos.base = {
    boot = {
      tmp = {
        cleanOnBoot = true;
      };

      loader.grub.enable = mkDefault true;
    };

    hardware.enableRedistributableFirmware = true;
  };

  flake.modules.nixos.efi = {
    boot.loader = {
      grub = {
        efiSupport = true;
        devices = ["nodev"];
      };

      efi.canTouchEfiVariables = mkDefault true;
      efi.efiSysMountPoint = mkDefault "/boot";
    };
  };
}
