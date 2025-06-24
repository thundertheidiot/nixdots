{
  disko.devices = {
    nodev."/" = {
      fsType = "tmpfs";
      mountOptions = [
        "size=10M"
        "defaults"
        "mode=755"
      ];
    };
    disk.main = {
      # Add device here
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
            size = "300M";
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
                "@home" = {
                  mountOptions = [];
                  mountpoint = "/home";
                };
                "@persist" = {
                  mountOptions = [];
                  mountpoint = "/nix/persist";
                };
                "@nix" = {
                  mountOptions = [];
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
    "/persist".neededForBoot = true;
    "/tmp".neededForBoot = true;
  };
}
