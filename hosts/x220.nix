{
  systemArch = "x86_64-linux";
  username = "thunder";
  homeDirectory = "/home/thunder";

  system = {
    config,
    lib,
    mlib,
    pkgs,
    modulesPath,
    ...
  }: {
    system.stateVersion = "23.05";

    time.timeZone = "Europe/Helsinki";
    networking.hostName = "x220";

    meow = {
      workstation = {
        enable = true;
        environment = [];
        # displayManager = "gdm";
      };

      users = "thunder";

      workstation.flatpak.graphicalStore = true;

      firefox.enable = true;
      emacs.enable = true;
      emacs.exwm = true;
      shell.enable = true;

      ssh.rootKey = true;

      # browser.enable = ["firedragon"];

      # browser.firefoxConfig.firedragon.profiles."nix-managed" = {
      #   id = 0;
      #   default = true;

      #   search = {
      #     force = true;
      #     default = "DuckDuckGo";
      #     privateDefault = "DuckDuckGo";
      #     engines = {
      #       "DuckDuckGo" = {
      #         urls = [
      #           {
      #             template = "https://duckduckgo.com/";
      #             params = [
      #               {
      #                 name = "q";
      #                 value = "{searchTerms}";
      #               }
      #             ];
      #           }
      #         ];
      #       };
      #       "Nix Packages" = {
      #         urls = [
      #           {
      #             template = "https://search.nixos.org/packages";
      #             params = [
      #               {
      #                 name = "query";
      #                 value = "{searchTerms}";
      #               }
      #             ];
      #           }
      #         ];
      #       };
      #       "Nix Options" = {
      #         urls = [
      #           {
      #             template = "https://search.nixos.org/options";
      #             params = [
      #               {
      #                 name = "query";
      #                 value = "{searchTerms}";
      #               }
      #             ];
      #           }
      #         ];
      #       };
      #       "Home Manager Options" = {
      #         urls = [
      #           {
      #             template = "https://home-manager-options.extranix.com/";
      #             params = [
      #               {
      #                 name = "query";
      #                 value = "{searchTerms}";
      #               }
      #             ];
      #           }
      #         ];
      #       };
      #     };
      #   };
      # };

      gpu = "intel";

      impermanence.enable = true;
      impermanence.persist = "/persist";

      monitors = [
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

    users.users.thunder.initialPassword = "password";

    boot.loader.grub.enable = true;
    # boot.loader.grub.device = "/dev/disk/by-uuid/d53a7434-16f0-4193-b057-4286168aea61";

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
                    mountpoint = "/persist";
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
      "/persist".neededForBoot = true;
      "/tmp".neededForBoot = true;
    };

    swapDevices = [];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
