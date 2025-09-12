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
    disk."main" = {
      device = "/dev/vda";
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
            size = "150M";
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
                "/nix" = {
                  mountOptions = ["compress=zstd" "noatime"];
                  mountpoint = "/nix";
                };
                "/persist" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/nix/persist";
                };
                "/tmp" = {
                  mountpoint = "/tmp";
                };
                "/var/tmp" = {
                  mountpoint = "/tmp";
                };
                "/storage" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/mnt/storage";
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
}
