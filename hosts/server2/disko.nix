{pkgs, ...}: {
  disko.devices = {
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "size=10M"
        "defaults"
        "mode=755"
      ];
    };
    disk."ssd2" = {
      device = "/dev/disk/by-id/ata-HFS256G39TND-N210A_EI73N07081120416G";
      type = "disk";
      content = {
        type = "gpt";
        partitions.main = {
          name = "ssd2";
          size = "100%";
          content = {
            type = "btrfs";
            subvolumes = {
              "/storage" = {
                mountOptions = ["compress=zstd"];
                mountpoint = "/mnt/ssd2";
              };
            };
          };
        };
      };
    };
    disk."ssd1" = {
      device = "/dev/disk/by-id/ata-SAMSUNG_MZHPV256HDGL-00000_S1X2NYAH700146";
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
            name = "os";
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
                "/var/tmp" = {
                  mountpoint = "/tmp";
                };
                "/storage" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/mnt/ssd1";
                };
              };
            };
          };
        };
      };
    };
    disk."4tb" = {
      device = "/dev/disk/by-id/ata-WDC_WD40EFPX-68C6CN0_WD-WX82D25N2NJ5";
      type = "disk";
      content = {
        type = "gpt";
        partitions.main = {
          name = "4tb";
          size = "100%";
          content = {
            type = "btrfs";
            subvolumes = {
              "/storage" = {
                mountOptions = ["compress=zstd"];
                mountpoint = "/mnt/4tb";
              };
            };
          };
        };
      };
    };
  };

  environment.systemPackages = with pkgs; [mergerfs];

  fileSystems = {
    "/nix".neededForBoot = true;
    "/tmp".neededForBoot = true;

    "/mnt/2tb" = {
      device = "/dev/disk/by-uuid/6c29f48d-d7de-4745-960e-8a241c638fee";
      fsType = "btrfs";
    };

    "/mnt/storage" = {
      fsType = "fuse.mergerfs";
      device = "/mnt/4tb:/mnt/2tb:/mnt/ssd1:/mnt/ssd2";
      depends = [
        "/mnt/4tb"
        "/mnt/2tb"
        "/mnt/ssd1"
        "/mnt/ssd2"
      ];
      options = [
        # https://trapexit.github.io/mergerfs/latest/quickstart
        # > linux 6.6
        "cache.files=off"
        "func.getattr=newest"
        "dropcacheonclose=false"
        "category.create=msppfrd"
      ];
    };
  };
}
