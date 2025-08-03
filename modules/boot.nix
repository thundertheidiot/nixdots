{
  lib,
  mlib,
  config,
  ...
}: let
  inherit (mlib) mkEnOpt mkEnOptTrue;
  inherit (lib) mkIf mkMerge mkDefault;

  cfg = config.meow.boot;
in {
  options = {
    meow.boot = {
      enable = mkEnOptTrue "Grub as bootloader + other tweaks";
      efi = mkEnOpt "EFI";
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      boot = {
        loader.grub.enable = true;
      };
    })
    (mkIf cfg.efi {
      boot.loader = {
        grub = {
          efiSupport = true;
          devices = ["nodev"];
        };

        efi.canTouchEfiVariables = mkDefault true;
        efi.efiSysMountPoint = mkDefault "/boot";
      };
    })
  ];
}
