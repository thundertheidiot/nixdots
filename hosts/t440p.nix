{
  systemArch = "x86_64-linux";
  username = "thunder";
  homeDirectory = "/home/thunder";

  options = {
    config,
    pkgs,
    mlib,
    ...
  }: {
    config = {
      username = "thunder";
      hostName = "t440p";
      timeZone = "Europe/Helsinki";

      workstation.enable = true;
      workstation.utils = "generic/gtk";
      workstation.environment = ["hyprland"];

      setup.hyprland.extraAutostart = [];
      setup.gaming.enable = true;
      setup.tv.enable = false;
      setup.laptop.enable = true;
    };
  };

  system = {
    config,
    lib,
    pkgs,
    modulesPath,
    mlib,
    ...
  }: {
    system.stateVersion = "24.05";

    time.timeZone = "Europe/Helsinki";
    networking.hostName = "t440p";

    meow = {
      fullSetup = true;
      workstation.enable = true;

      gaming.enable = true;
      gaming.emulation = true;
      gaming.games = ["duckgame"];

      browser.firefoxConfig = {
        testfox = {
          configPath = "browsertest";
          profiles = {
            "nix-managed" = {
              id = 0;
              default = true;
            };
          };
        };
      };

      firefox.enable = true;
      emacs.enable = true;
      emacs.exwm = true;
      shell.enable = true;

      gpu = "intel";

      keyboard = {
        enable = true;
        devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
      };

      monitors = mlib.mkMonitors [
        {
          name = "eDP-1";
          width = "1920";
          height = "1080";
        }
      ];

      home = {
        stateVersion = "24.05";
      };
    };

    # environment.systemPackages = with pkgs; [
    #   (nyxt.override {
    #     sbclPackages = sbcl_2_4_6.pkgs;
    #   })
    # ];

    boot.loader.grub.enable = true;
    boot.loader.grub.device = "/dev/disk/by-id/ata-TOSHIBA_MQ01ABD100_97DOPOT9T";
    boot.loader.grub.useOSProber = true;

    boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
    boot.initrd.kernelModules = [];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    # boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;

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
