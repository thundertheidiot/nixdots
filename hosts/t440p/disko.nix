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

    disk.msata = {
      type = "disk";
      device = "/dev/disk/by-id/ata-TS240GMTS420S_E934040024";
      content = {
        type = "gpt";
        partitions = {
          main = {
            name = "msata";
            size = "100%";
            content = {
              type = "btrfs";
              subvolumes = {
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

    disk.ssd = {
      type = "disk";
      device = "/dev/disk/by-id/ata-KINGSTON_SA400S37240G_50026B7380C44F59";
      content = {
        type = "gpt";
        partitions = {
          grub = {
            size = "1M";
            type = "EF02";
          };
          boot = {
            size = "300M";
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
