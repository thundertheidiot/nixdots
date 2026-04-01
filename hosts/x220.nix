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
    workstation.environment = ["niri"];
    workstation.displayManager = "gdm";
    home.enable = true;
    user = "ella";

    emacs.enable = true;
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

  boot.loader.grub = {
    enable = true;
    enableCryptodisk = true;
    device = "nodev";
    # mirroredBoots = lib.mkForce [
    #   {
    #     devices = ["/dev/disk/by-id/ata-Samsung_SSD_860_EVO_250GB_S3YJNF0K236870M"];
    #     path = "/boot";
    #   }
    # ];
  };

  boot.initrd.luks.devices."cryptroot" = {
    device = "/dev/disk/by-partlabel/luks";
  };

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
      type = "disk";
      device = "/dev/disk/by-id/ata-Samsung_SSD_860_EVO_250GB_S3YJNF0K236870M";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            # this was a grub mbr thing, but it's not needed
            size = "1M";
          };
          # libreboot with grub payload
          luks = {
            size = "100%";
            label = "luks";
            content = {
              type = "luks";
              name = "cryptroot";
              settings = {
                allowDiscards = true;
                keyFile = "/tmp/disk.key";
              };
              extraOpenArgs = [
                # https://haseebmajid.dev/posts/2024-07-30-how-i-setup-btrfs-and-luks-on-nixos-using-disko/
                "--perf-no_read_workqueue"
                "--perf-no_write_workqueue"
              ];
              content = {
                type = "btrfs";

                subvolumes = {
                  "/boot" = {
                    mountpoint = "/boot";
                  };
                  "@home" = {
                    mountOptions = ["compress=zstd"];
                    mountpoint = "/home";
                  };
                  "@nix" = {
                    mountOptions = ["compress=zstd" "noatime"];
                    mountpoint = "/nix";
                  };
                  "@persist" = {
                    mountOptions = ["compress=zstd"];
                    mountpoint = "/nix/persist";
                  };
                  "@tmp" = {
                    mountpoint = "/tmp";
                  };
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
