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
      hostName = "desktop";
      timeZone = "Europe/Helsinki";

      # tv.enable = true;

      workstation.enable = true;
      workstation.utils = "generic/gtk";
      workstation.plasma.tilingwm = true;
      workstation.environment = "hyprland";

      setup.userMachine.enable = true;
      setup.swayfx.enable = true;
      setup.hyprland.enable = true;
      setup.hyprland.extraConfig = ''
        monitor=DP-3, 2560x1440@144, 1920x0, 1, vrr,2
        monitor=DP-1, 1920x1080@144, 0x0, 1, vrr,0
        source=~/.config/hypr/crt.conf
      '';
      setup.hyprland.extraAutostart = [
        "${pkgs.ckb-next}/bin/ckb-next -b"
      ];
      setup.awesomeWM.enable = true;
      setup.firefox.enable = true;
      setup.gaming.enable = true;
      setup.tv.enable = true;

      setup.desktop.enable = true;

      setup.gpu = "amd";

      monitors = [
        {
          name = "DP-3";
          width = "2560";
          height = "1440";
          refresh = "144";
          x = "1920";
        }
        {
          name = "DP-1";
          width = "1920";
          height = "1080";
          refresh = "144";
        }
      ];
    };
  };

  home = {...}: {
    home.stateVersion = "24.05";

    gtk.gtk3.bookmarks = [
      "file:///mnt/4tb"
    ];
  };

  system = {
    lib,
    config,
    mlib,
    ...
  }: {
    system.stateVersion = "24.05";

    boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
    # boot.initrd.kernelModules = ["amdgpu"];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    # services.xserver.enable = true;
    # services.xserver.videoDrivers = ["amdgpu"];

    # hardware.opengl.enable = true;
    # hardware.opengl.driSupport = true;
    # hardware.opengl.driSupport32Bit = true;

    # hardware.enableRedistributableFirmware = true;

    hardware.ckb-next.enable = true;

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    boot.kernelParams = [
      "video=1920x1080-32"
    ];

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

    fileSystems."/mnt/4tb" = {
      device = "/dev/disk/by-uuid/03606f24-3ecb-41d3-b918-bfe580452e30";
      fsType = "btrfs";
      options = ["compress=zstd"];
    };

    swapDevices = [];

    networking.useDHCP = lib.mkDefault true;

    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
