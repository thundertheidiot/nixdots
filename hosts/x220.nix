{
  config,
  lib,
  mlib,
  pkgs,
  modulesPath,
  ...
}: {
  system.stateVersion = "26.05";

  time.timeZone = "Europe/Helsinki";
  networking.hostName = "x220";

  home-manager.sharedModules = [
    {
      home.stateVersion = "26.05";

      mHome.browser.firefox.enable = true;
      mHome.setup.fullLanguages = true;
    }
  ];

  nixpkgs.config.allowUnfree = lib.mkForce false;

  meow = {
    ssh.key = true;
    ssh.rootKey = true;

    workstation.enable = true;
    workstation.displayManager = "gdm";
    home.enable = true;
    user = "ella";

    emacs.enable = true;
    emacs.ewm.enable = true;
    shell.enable = true;

    gpu = "intel";

    impermanence.enable = true;
    impermanence.persist = "/nix/persist";

    monitors."LVDS-1" = {
      width = 1366;
      height = 768;
      primary = true;
    };

    keyboard = {
      enable = true;
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
    };
  };

  users.users.ella.initialPassword = "password";

  boot.loader.grub.enable = true;
  # boot.loader.grub.device = "/dev/disk/by-uuid/d53a7434-16f0-4193-b057-4286168aea61";

  boot.kernelParams = ["iomem=relaxed"];

  boot.initrd.availableKernelModules = ["ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  disko.devices = {
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "size=10M"
        "defaults"
        "mode=755"
      ];
    };
    disk.ssd = {
      device = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_250GB_S3YJNF0K236870M";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            name = "boot";
            size = "1M";
            type = "EF02";
          };
          efi = {
            size = "500M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          main = {
            name = "main";
            size = "100%";
            content = {
              type = "btrfs";
              subvolumes = {
                "/home" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/home";
                };
                "/persist" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/nix/persist";
                };
                "/nix" = {
                  mountOptions = ["compress=zstd" "noatime"];
                  mountpoint = "/nix";
                };
                "/tmp" = {
                  mountpoint = "/tmp";
                };
              };
            };
          };
        };
      };
    };
  };

  fileSystems = {
    "/nix".neededForBoot = true;
    "/nix/persist".neededForBoot = true;
    "/tmp".neededForBoot = true;
  };

  swapDevices = [];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
