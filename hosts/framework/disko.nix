{...}: {
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
      device = "/dev/disk/by-id/nvme-ADATA_LEGEND_800_2O412921QJL1";
      content = {
        type = "gpt";
        partitions = {
          boot = {
            size = "500M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          main = {
            name = "ssd";
            size = "100%";
            content = {
              type = "btrfs";
              subvolumes = {
                "@storage" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/mnt/storage";
                };
                "@persist" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/nix/persist";
                };
                "@nix" = {
                  mountOptions = ["compress=zstd" "noatime"];
                  mountpoint = "/nix";
                };
                "@tmp" = {
                  mountpoint = "/tmp";
                };
                "@var_tmp" = {
                  mountpoint = "/var/tmp";
                };
                "@home" = {
                  mountOptions = ["compress=zstd"];
                  mountpoint = "/home";
                };
              };
            };
          };
        };
      };
    };
  };

  meow.impermanence.persist = "/nix/persist";

  fileSystems = {
    "/nix".neededForBoot = true;
    "/nix/persist".neededForBoot = true;
    "/tmp".neededForBoot = true;
  };
}
