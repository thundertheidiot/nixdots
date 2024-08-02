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
      hostName = "x220";
      timeZone = "Europe/Helsinki";

      workstation.enable = true;
      workstation.laptop = false;
      workstation.utils = "generic/gtk";
      workstation.environment = ["hyprland"];
      workstation.plasma.tilingwm = true;

      setup.hyprland.extraAutostart = [];
      setup.gaming.enable = false;
      setup.tv.enable = true;
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
    system.stateVersion = "23.05";

    meow = {
      firefox.enable = true;
      emacs.enable = true;

      gpu = "intel";

      monitors = [
        {
          name = "LVDS-1";
          width = 1366;
          height = 768;
        }
      ];

      keyboard.enable = true;
    };

    boot.loader.grub.enable = true;
    boot.loader.grub.device = "/dev/sda";

    boot.initrd.availableKernelModules = ["ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci"];
    boot.initrd.kernelModules = [];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/d53a7434-16f0-4193-b057-4286168aea61";
      fsType = "btrfs";
    };

    swapDevices = [];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
