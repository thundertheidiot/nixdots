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
      # setup.gaming.enable = false;
      # setup.tv.enable = true;
    };
  };

  system = {
    config,
    lib,
    mlib,
    pkgs,
    modulesPath,
    ...
  }: {
    system.stateVersion = "23.05";

    meow = {
      firefox.enable = true;
      emacs.enable = true;
      shell.enable = true;

      gpu = "intel";

      monitors = mlib.mkMonitors [
        {
          name = "LVDS-1";
          width = 1366;
          height = 768;
        }
      ];

      keyboard = {
        enable = true;
        devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
      };

      home = {
        stateVersion = "24.05";
      };
    };

    boot.loader.grub.enable = true;
    boot.loader.grub.device = "/dev/sda";

    boot.initrd.availableKernelModules = ["ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci"];
    boot.initrd.kernelModules = [];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    # fileSystems."/" = {
    #   device = "/dev/disk/by-uuid/d53a7434-16f0-4193-b057-4286168aea61";
    #   fsType = "btrfs";
    # };

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
        device = "/dev/disk/by-uuid/d53a7434-16f0-4193-b057-4286168aea61";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              name = "boot";
              size = "1M";
              type = "EF02";
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

    swapDevices = [];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
