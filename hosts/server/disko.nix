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
    disk."msata" = {
      device = "/dev/disk/by-id/ata-KINGSTON_SUV500M8240G_50026B7683581D58";
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
                  mountpoint = "/mnt/msata";
                };
              };
            };
          };
        };
      };
    };
    disk."1tb" = {
      device = "/dev/disk/by-id/ata-KINGSTON_SA400S37960G_50026B738339C106";
      type = "disk";
      content = {
        type = "gpt";
        partitions.main = {
          name = "storage";
          size = "100%";
          content = {
            type = "btrfs";
            subvolumes = {
              "/persist" = {
                mountOptions = ["compress=zstd"];
                mountpoint = "/nix/persist";
              };
              "/storage" = {
                mountOptions = ["compress=zstd"];
                mountpoint = "/mnt/1tb";
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
    "/nix/persist".neededForBoot = true;
    "/tmp".neededForBoot = true;

    "/mnt/storage" = {
      fsType = "fuse.mergerfs";
      device = "/mnt/1tb:/mnt/msata";
      depends = [
        "/mnt/1tb"
        "/mnt/msata"
      ];
      options = [
        "cache.files=auto-full"
        "dropcacheonclose=true"
        "category.create=mfs"
      ];
    };
  };
}
