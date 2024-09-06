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
      workstation.plasma.tilingwm = true;
      workstation.environment = ["hyprland" "plasma"];

      setup.hyprland.extraAutostart = [
        "${pkgs.ckb-next}/bin/ckb-next -b"
      ];
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
    system.stateVersion = "24.05";

    boot.initrd.availableKernelModules = ["xhci_pci" "ahci" "nvme" "usbhid" "usb_storage" "sd_mod"];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    boot.kernelParams = [
      "video=1920x1080-32"
    ];

    virtualisation.docker.enable = true;

    services.ollama = {
      acceleration = "rocm";
      environmentVariables = {
        HCC_AMDGPU_TARGET = "gfx1031";
      };
      rocmOverrideGfx = "10.3.1";
    };

    meow = {
      fullSetup = true;
      workstation.enable = true;

      gaming.enable = true;
      gaming.emulation = true;
      gaming.games = ["duckgame" "minecraft"];

      firefox.enable = true;
      emacs.enable = true;
      emacs.exwm = true;
      shell.enable = true;

      gpu = "amd";

      virtualization.enable = true;

      # tv.enable = true;

      keyboard.enable = false;
      keyboard.devices = ["/dev/input/by-id/usb-YMDK_YD60MQ-if01-event-kbd"];

      monitors = mlib.mkMonitors [
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
            ];
          })
        ];
      };
    };

    environment.systemPackages = with pkgs; [
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
    boot.binfmt.emulatedSystems = ["aarch64-linux"];
    users.users.${config.username}.extraGroups = ["adbusers"];

    hardware.ckb-next.enable = true;

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/1ffc3323-6810-406d-b4f6-15d247602689";
      fsType = "btrfs";
      options = ["subvol=@"];
    };

    fileSystems."/boot" = {
      device = "/dev/disk/by-uuid/F33A-5CD4";
      fsType = "vfat";
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

    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
