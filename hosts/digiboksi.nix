{
  lib,
  pkgs,
  config,
  modulesPath,
  mlib,
  ...
}: {
  # TODO resetup digiboksi completely
  time.timeZone = "Europe/Helsinki";
  networking.hostName = "digiboksi";

  home-manager.sharedModules = [
    {
      home.stateVersion = "24.05";
    }
  ];

  meow = {
    user = "tv";

    home.enable = true;
    workstation.enable = true;
    workstation.environment = ["hyprland"];

    monitors."HDMI-A-1" = {
      width = 1360;
      height = 768;
    };

    gpu = "intel";

    old-tv.enable = true;

    emacs.enable = true;
    shell.enable = true;
  };

  system.stateVersion = "23.11";

  boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  boot.loader.grub = {
    enable = true;
    device = "/dev/sda";
    extraConfig = ''
      set timeout=1
    '';
  };

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
}
