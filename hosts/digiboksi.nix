{
  systemArch = "x86_64-linux";
  username = "tv";
  homeDirectory = "/home/tv";

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

    time.timeZone = "Europe/Helsinki";
    networking.hostName = "digiboksi";

    meow = {
      user = "tv";

      monitors = [
        {
          name = "HDMI-A-1";
          width = 1360;
          height = 768;
        }
      ];

      gpu = "intel";

      tv.enable = true;

      emacs.enable = true;
      shell.enable = true;

      firefox = {
        enable = true;
        addons = {
          "uBlock0@raymonhill.net" = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/addon-11423598-latest.xpi";
          "idcac-pub@guus.ninja" = "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/addon-17568914-latest.xpi";
        };
      };

      home = {
        stateVersion = "24.05";
      };
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
