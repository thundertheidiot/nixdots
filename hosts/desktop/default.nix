[
  "base"
  "plasma"
  "plasma/basic"
  ({
    lib,
    config,
    pkgs,
    ...
  }: {
    imports = [
      ./vr.nix
      ./rathole.nix
      # ./firedragon.nix
    ];

    system.stateVersion = "24.05";

    boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    boot.kernelParams = [
      "video=1920x1080-32"
      "video=DP-3:D"
      "video=DP-1:D"
      "video=HDMI-A-1:D"
      "amdgpu.ppfeaturemask=0xffffffff"
    ];

    boot.supportedFilesystems = [
      "ntfs"
      "e2fs"
    ];

    virtualisation.docker.enable = true;

    time.timeZone = "Europe/Helsinki";
    networking.hostName = "desktop";

    programs.corectrl.enable = true;
    hardware.amdgpu.overdrive.enable = true;
    hardware.amdgpu.overdrive.ppfeaturemask = "0xffffffff";
    programs.corectrl.gpuOverclock.enable = true;

    home-manager.sharedModules = [
      {
        home.stateVersion = "24.05";
        mHome.browser.zen.enable = true;
        mHome.emacs.enable = true;

        gtk.gtk3.bookmarks = [
          # "file:///mnt/4tb"
          "file:///mnt/1tb_nvme"
        ];

        wayland.windowManager.hyprland.settings.render = {
          explicit_sync = 1;
          explicit_sync_kms = 1;
        };

        services.syncthing = {
          enable = true;
        };
      }
    ];

    meow = {
      fullSetup = true;
      workstation.enable = true;

      user = "thunder";

      workstation.environment = ["plasma"];
      workstation.plasma.opinionatedConfig = true;
      workstation.hyprland.extraAutostart = [
        "${pkgs.ckb-next}/bin/ckb-next -b"
      ];

      workstation.flatpak.graphicalStore = true;

      gaming.enable = true;
      gaming.emulation = true;
      gaming.games = ["duckgame" "minecraft"];

      emacs.enable = true;
      emacs.exwm = true;
      shell.enable = true;

      gpu = "amd";

      virtualization.enable = true;

      keyboard.enable = false;
      keyboard.devices = ["/dev/input/by-id/usb-YMDK_YD60MQ-if01-event-kbd"];

      monitors = [
        {
          name = "DP-3";
          width = 2560;
          height = 1440;
          refresh = 144;
          x = 1920;
          hyprlandExtra = "vrr, 0";
          primary = true;
          xorgModeline = ''Modeline "2560x1440x143.9"  592.00  2560 2568 2600 2666  1440 1465 1473 1543 +hsync -vsync'';
        }
        {
          name = "DP-1";
          width = 1920;
          height = 1080;
          refresh = 144;
          hyprlandExtra = "vrr, 0";
          xorgModeline = ''Modeline "1920x1080x144.0"  325.08  1920 1944 1976 2056  1080 1083 1088 1098 +hsync +vsync'';
        }
        {
          name = "HDMI-A-1";
          hyprlandExclude = true;
          x = 4480;
          y = 0;
          xorgName = "HDMI-1";
          edid = ./crt-edited.bin;
          customModes = [
            {
              real = "512x448@120.00";
              display = "SNES 512x448";
            }
            {
              real = "640x480@120.01";
              display = "EMU 640x480@120";
            }
            {
              real = "640x528@120.02";
              display = "GC 640x528@120";
            }
            {
              real = "480x320@120.00";
              display = "GBA 480x320@120";
            }
            {
              real = "640x480@144.87";
              display = "640x480@144";
            }
            {
              real = "800x600@112.51";
              display = "800x600@112";
            }
            {
              real = "1024x768@90.57";
              display = "1024x768@90";
            }
            {
              real = "1280x1024@67.02Hz";
              display = "1280x1024@67";
            }
            {
              real = "960x540@128.00";
              display = "WIDE 960x540@128";
            }
            {
              real = "1280x720@96.00Hz";
              display = "WIDE 1280x720@96";
            }
          ];
        }
      ];
    };

    hardware.keyboard.qmk.enable = true;

    environment.systemPackages = with pkgs; [
      pcsx2

      distrobox

      easyeffects
      deepfilternet

      syncthing

      krita

      gpu-screen-recorder-gtk

      qmk
      android-tools

      mpkgs.bambustudio
      orca-slicer
    ];

    programs.adb.enable = true;
    # boot.binfmt.emulatedSystems = ["aarch64-linux"];
    users.users."${config.meow.user}".extraGroups = ["adbusers"];

    hardware.ckb-next.enable = true;

    boot.loader = {
      grub = {
        enable = true;
        efiSupport = true;
        devices = ["nodev"];
      };
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot";
    };

    meow.impermanence.enable = true;
    meow.impermanence.persist = "/persist";

    zramSwap = {
      enable = true;
      algorithm = "zstd";
      priority = 100;
      memoryPercent = 75;
    };

    boot.kernel.sysctl."vm.swappiness" = 50;

    networking.firewall.allowedTCPPorts = [
      3002 # might be orca slicer, i forgot
      8188 # comfyui
    ];

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
        device = "/dev/disk/by-id/nvme-ADATA_LEGEND_800_2N452L4H5END";
        content = {
          type = "gpt";
          partitions = {
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
                  "/storage" = {
                    mountOptions = ["compress=zstd"];
                    mountpoint = "/mnt/1tb_nvme";
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
                  "/var_tmp" = {
                    mountpoint = "/var/tmp";
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

    swapDevices = [
      {
        device = "/mnt/1tb_nvme/swapfile";
        priority = -2;
      }
    ];

    fileSystems."/home" = {
      device = "/dev/disk/by-uuid/8e5420d3-a33b-4de5-a06f-267202f1b3ee";
      fsType = "btrfs";
      options = ["subvol=/subvolumes/home"];
    };

    # fileSystems."/mnt/4tb" = {
    #   device = "/dev/disk/by-uuid/03606f24-3ecb-41d3-b918-bfe580452e30";
    #   fsType = "btrfs";
    #   options = ["compress=zstd"];
    # };

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  })
]
