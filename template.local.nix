{
  options = {
    config,
    pkgs,
    ...
  }: {
    username = "thunder";
    hostName = "desktop";
    timeZone = "Europe/Helsinki";

    setup.userMachine.enable = true;
    setup.hyprland.enable = true;
    setup.hyprland.extraConfig = ''
      monitor=DP-3, 2560x1440@144, 1920x0, 1
      monitor=DP-1, 1920x1080@144, 0x0, 1
    '';
    setup.hyprland.extraAutostart = [
      "${pkgs.ckb-next}/bin/ckb-next -b"
    ];
    setup.awesomeWM.enable = true;
    setup.firefox.enable = true;
    setup.gaming.enable = true;
    setup.tv.enable = false;

    setup.gpu = "amd";
  };

  home = { ... }: {
    home.stateVersion = "24.05";
  };

  system = {
    lib,
    config,
    ...
  }: {
    system.stateVersion = "24.05";

    boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
    boot.initrd.kernelModules = ["amdgpu"];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    services.xserver.enable = true;
    services.xserver.videoDrivers = ["amdgpu"];

    hardware.opengl.enable = true;
    hardware.opengl.driSupport = true;
    hardware.opengl.driSupport32Bit = true;

    hardware.enableRedistributableFirmware = true;

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/1ffc3323-6810-406d-b4f6-15d247602689";
      fsType = "btrfs";
      options = ["subvol=@"];
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/F33A-5CD4";
      fsType = "vfat";
    };

    fileSystems."/home" = {
      device = "/dev/disk/by-uuid/8e5420d3-a33b-4de5-a06f-267202f1b3ee";
      fsType = "btrfs";
      options = ["subvol=/subvolumes/home"];
    };

    swapDevices = [];

    networking.useDHCP = true;

    # nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
