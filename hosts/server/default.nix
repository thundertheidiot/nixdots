{
  systemArch = "x86_64-linux";
  username = "thunder";
  homeDirectory = "/home/thunder";

  # old cruft
  # TODO clean all this shit
  options = {
    workstation.enable = false;
    workstation.utils = "none";
    workstation.environment = [];
  };

  system = {
    config,
    lib,
    mlib,
    pkgs,
    modulesPath,
    ...
  }: let
    inherit (mlib) mkOpt;
    inherit (lib.types) str;

    cfg = config.server;
  in {
    imports = [
      ./options.nix

      ./new-torrent

      ./dns.nix
      ./webserver.nix
      ./rathole.nix

      ./jellyfin.nix
      ./vaultwarden.nix
      ./homepage.nix
      # ./torrent
    ];

    options = {
      # here for easier changing in case of router change etc.
      server.addr = mkOpt str "192.168.101.101" {};
    };

    config = {
      system.stateVersion = "24.05";

      time.timeZone = "Europe/Helsinki";
      networking.hostName = "server";

      meow = {
        workstation.enable = false;
        shell.enable = true;

        ssh.rootKey = true;

        gpu = "intel";

        impermanence.enable = true;
        impermanence.persist = "/persist";

        home = {
          stateVersion = "24.05";
        };
      };

      nix.gc = {
        automatic = true;
        dates = "daily";
        options = "--delete-older-than 7d";
      };

      users.users.thunder.initialPassword = "password";

      boot.loader.grub.enable = true;

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
          device = "/dev/disk/by-id/ata-Samsung_SSD_850_EVO_250GB_S21PNXAG912243J";
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
  };
}
