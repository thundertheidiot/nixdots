{
  systemArch = "x86_64-linux";
  username = "tv";
  homeDirectory = "/home/tv";

  options = {
    config,
    pkgs,
    ...
  }: {
    config = {
      username = "tv";
      hostName = "digiboksi";
      timeZone = "Europe/Helsinki";

      workstation.enable = true;
      workstation.utils = "generic/gtk";
      workstation.environment = ["hyprland"];

      setup.firefox.enable = true;
      setup.gaming.enable = false;
      setup.tv.enable = true;

      setup.gpu = "intel";
    };
  };

  home = {...}: {
    home.stateVersion = "24.05";
  };

  system = {
    lib,
    pkgs,
    config,
    modulesPath,
    mlib,
    ...
  }: {
    # Machine specific configuration, filesystems, bootloader, basically hardware-configuration.nix + system.stateVersion
    boot.loader.grub = {
      enable = true;
      device = "/dev/sda";
      extraConfig = ''
        set timeout=1
      '';
    };

    meow = {
      monitors = mlib.mkMonitors [
        {
          name = "HDMI-A-1";
          width = 1360;
          height = 768;
        }
      ];
    };

    hardware.graphics.enable = true;

    system.stateVersion = "23.11";

    boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
    boot.initrd.kernelModules = [];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/721918a3-0d55-4b0b-b531-484b224568a1";
      fsType = "btrfs";
      options = ["subvol=@"];
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/157d9ce8-9d0f-408f-b489-2478a285ef7b";
      fsType = "ext4";
    };

    swapDevices = [];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
