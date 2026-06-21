{...}: {
  disko.devices = {
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "size=2048M"
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
            size = "1024M";
            type = "EF00";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot";
            };
          };
          luks = {
            size = "100%";
            content = {
              type = "luks";
              name = "ssd";
              settings = {
                allowDiscards = true;
              };
              extraOpenArgs = [
                # https://haseebmajid.dev/posts/2024-07-30-how-i-setup-btrfs-and-luks-on-nixos-using-disko/
                "--perf-no_read_workqueue"
                "--perf-no_write_workqueue"
              ];
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
  };

  meow.impermanence.persist = "/nix/persist";

  fileSystems = {
    "/nix".neededForBoot = true;
    "/nix/persist".neededForBoot = true;
    "/tmp".neededForBoot = true;
  };
}
