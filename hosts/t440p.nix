{
  systemArch = "x86_64-linux";
  username = "thunder";
  homeDirectory = "/home/thunder";

  options = {
    config,
    pkgs,
    ...
  }: {
    config = {
      username = "thunder";
      hostName = "t440p";
      timeZone = "Europe/Helsinki";

      setup.userMachine.enable = true;
      setup.hyprland.enable = true;
      setup.hyprland.extraConfig = ''
        monitor=eDP-1,1920x1080@60,0x0,1
      '';
      setup.hyprland.extraAutostart = [];
      setup.awesomeWM.enable = true;
      setup.firefox.enable = true;
      setup.gaming.enable = false;
      setup.tv.enable = false;
      setup.laptop.enable = true;

      monitors = [
        {
          name = "LVDS-1";
          width = 1366;
          height = 768;
        }
      ];

      setup.gpu = "intel";
    };
  };

  home = {...}: {
    home.stateVersion = "24.05";
  };

  system = {
    config,
    lib,
    pkgs,
    modulesPath,
    ...
  }: {
    system.stateVersion = "24.05";

    boot.loader.grub.enable = true;
    boot.loader.grub.device = "/dev/disk/by-id/ata-TOSHIBA_MQ01ABD100_97DOPOT9T";
    boot.loader.grub.useOSProber = true;

    boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
    boot.initrd.kernelModules = [];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/85995899-d984-4241-87b4-2fc77e05f66f";
      fsType = "btrfs";
      options = ["subvol=@"];
    };

    fileSystems."/mnt/extra" = {
      device = "/dev/disk/by-uuid/07f2d41e-c9a9-4b30-a4a2-33844b8449b4";
      fsType = "btrfs";
    };

    swapDevices = [
      {device = "/dev/disk/by-uuid/5bef274b-32b4-456d-a937-5eb08b158735";}
    ];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
