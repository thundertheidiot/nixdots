let
  systemArch = "x86_64-linux";
  username = "thunder";
  homeDirectory = "/home/thunder";
in {
  inherit systemArch username homeDirectory;

  options = {
    config,
    pkgs,
    mlib,
    ...
  }: {
    config = {
      username = username;
      hostName = "desktop";
      timeZone = "Europe/Helsinki";

      # tv.enable = true;

      workstation.enable = true;
      workstation.utils = "generic/gtk";
      workstation.environment = ["hyprland"];

      setup.gaming.enable = true;
      setup.tv.enable = false;

      setup.desktop.enable = true;
    };
  };

  system = {
    lib,
    config,
    mlib,
    pkgs,
    ...
  }: {
    imports = [
      ./vr.nix
      ./rathole.nix
    ];

    system.stateVersion = "24.05";

    boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    boot.kernelParams = [
      "video=1920x1080-32"
    ];

    boot.supportedFilesystems = [
      "ntfs"
      "e2fs"
    ];

    virtualisation.docker.enable = true;

    time.timeZone = "Europe/Helsinki";
    networking.hostName = "desktop";

    services.ollama = {
      enable = true;
      acceleration = "rocm";
      environmentVariables = {
        HCC_AMDGPU_TARGET = "gfx1031";
      };
      rocmOverrideGfx = "10.3.1";
    };

    meow = {
      fullSetup = true;
      workstation.enable = true;

      workstation.environment = ["hyprland" "plasma"];
      workstation.plasma.opinionatedConfig = true;
      workstation.plasma.tiling = true;
      workstation.hyprland.extraAutostart = [
        "${pkgs.ckb-next}/bin/ckb-next -b"
      ];

      workstation.flatpak.graphicalStore = true;

      browser.enable = ["firedragon"];

      browser.firefoxConfig.firedragon.profiles."nix-managed" = {
        id = 0;
        default = true;

        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
          engines = {
            "DuckDuckGo" = {
              urls = [
                {
                  template = "https://duckduckgo.com/";
                  params = [
                    {
                      name = "q";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
            "Nix Packages" = {
              urls = [
                {
                  template = "https://search.nixos.org/packages";
                  params = [
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
            "Nix Options" = {
              urls = [
                {
                  template = "https://search.nixos.org/options";
                  params = [
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
            "Home Manager Options" = {
              urls = [
                {
                  template = "https://home-manager-options.extranix.com/";
                  params = [
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
          };
        };
      };

      gaming.enable = true;
      gaming.emulation = true;
      gaming.games = ["duckgame" "minecraft"];

      firefox.enable = true;
      emacs.enable = true;
      emacs.llm = true;
      emacs.exwm = true;
      shell.enable = true;

      gpu = "amd";

      virtualization.enable = true;

      # tv.enable = true;

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

      home = {
        stateVersion = "24.05";
        modules = [
          ({...}: {
            gtk.gtk3.bookmarks = [
              "file:///mnt/4tb"
              "file:///mnt/1tb_nvme"
            ];
          })
        ];
      };
    };

    environment.systemPackages = with pkgs; [
      qmk
      android-tools
      (flashprint.overrideAttrs (prev: rec {
        version = "5.8.6";

        src = fetchurl {
          url = "http://www.ishare3d.com/3dapp/public/FlashPrint-5/FlashPrint/flashprint5_${version}_amd64.deb";
          hash = "sha256-oi/nEdOjhbYf9IZmppfKiEmlNGXdc907LS2x8jUck+M=";
        };

        nativeBuildInputs = prev.nativeBuildInputs ++ [pkgs.makeWrapper];

        installPhase =
          builtins.replaceStrings
          ["runHook postInstall"]
          ["wrapProgram $out/bin/flashprint --set HOME ${config.stubbornHomeDirectory}\nrunHook postInstall"]
          prev.installPhase;
      }))
    ];

    programs.adb.enable = true;
    # boot.binfmt.emulatedSystems = ["aarch64-linux"];
    users.users.${config.username}.extraGroups = ["adbusers"];

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
      memoryPercent = 50;
    };

    boot.kernel.sysctl."vm.swappiness" = 180;

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

    fileSystems."/home" = {
      device = "/dev/disk/by-uuid/8e5420d3-a33b-4de5-a06f-267202f1b3ee";
      fsType = "btrfs";
      options = ["subvol=/subvolumes/home"];
    };

    fileSystems."/mnt/4tb" = {
      device = "/dev/disk/by-uuid/03606f24-3ecb-41d3-b918-bfe580452e30";
      fsType = "btrfs";
      options = ["compress=zstd"];
    };

    swapDevices = [];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
