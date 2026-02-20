{
  lib,
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    ./vr.nix
    ./rathole.nix
    # ./firedragon.nix
    inputs.nixos-hardware.nixosModules.common-pc-ssd
    inputs.nixos-hardware.nixosModules.common-gpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
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
    # "amdgpu.ppfeaturemask=0xffffffff"
  ];

  boot.supportedFilesystems = [
    "ntfs"
    "e2fs"
  ];

  boot.kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-latest-lto-zen4;

  virtualisation.docker.enable = true;

  time.timeZone = "Europe/Helsinki";
  networking.hostName = "desktop";

  programs.corectrl.enable = true;
  # hardware.amdgpu.overdrive.enable = true;
  # hardware.amdgpu.overdrive.ppfeaturemask = "0xffffffff";

  # the finals fix
  security.pam.loginLimits = [
    {
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "200000";
    }
    {
      domain = "*";
      type = "hard";
      item = "nofile";
      value = "200000";
    }
  ];

  systemd.user.extraConfig = "DefaultLimitNOFILE=200000:200000";

  systemd.settings.Manager = {
    DefaultLimitNOFILE = "200000:200000";
  };

  home-manager.sharedModules = [
    {
      home.stateVersion = "24.05";
      mHome.browser.firefox.enable = true;
      mHome.lang.latex = true;
      meowEmacs.enable = true;

      gtk.gtk3.bookmarks = [
        # "file:///mnt/4tb"
        "file:///mnt/1tb_nvme"
      ];

      services.syncthing = {
        enable = true;
      };
    }
  ];

  meow = {
    fullSetup = true;
    workstation.enable = true;

    boot.efi = true;

    user = "thunder";

    workstation.environment = ["niri"];
    workstation.hyprland.extraAutostart = [
      "${pkgs.ckb-next}/bin/ckb-next -b"
    ];

    workstation.extraWaybarModules = {
      "custom/qbittorrent" = let
        script = pkgs.stdenv.mkDerivation {
          name = "qbittorrent-waybar";

          propagatedBuildInputs = [
            (pkgs.python3.withPackages (pkgs: with pkgs; [humanize requests]))
          ];

          dontUnpack = true;

          installPhase = ''
            install -Dm755 ${./qbit.py} $out/bin/qbit
          '';
        };
      in {
        exec = "${script}/bin/qbit";
        on-click = "${script}/bin/qbit toggle_limit";
        on-click-right = "xdg-open https://torrent.home";
        return-type = "json";
        restart-interval = "1";
        format = "{icon} {text}";
        format-icons = {
          normal = "<span foreground='green'>󰓅</span> ";
          alternative = "<span foreground='red'>󰾆</span> ";
        };
      };
    };

    gaming.enable = true;
    gaming.emulation = true;
    gaming.games = ["duckgame" "minecraft"];

    emacs.enable = true;
    shell.enable = true;

    school.enable = true;

    gpu = "amd";

    virtualization.enable = true;

    keyboard.enable = false;
    keyboard.devices = ["/dev/input/by-id/usb-YMDK_YD60MQ-if01-event-kbd"];

    workstation.extraNiriConfig = [
      ''
        workspace "chat" {
          open-on-output "DP-1"
        }

        window-rule {
          match at-startup=true app-id="vesktop"
          match at-startup=true app-id="signal"
          match at-startup=true app-id="cinny"
          match at-startup=true app-id=r#"^org\.gajim\.Gajim$"#

          open-on-workspace "chat"
          open-maximized true
        }

        spawn-at-startup "vesktop"
        spawn-at-startup "signal-desktop"
        spawn-at-startup "gajim"
        spawn-at-startup "in.cinny.Cinny"
      ''
    ];

    monitors = {
      "DP-3" = {
        width = 2560;
        height = 1440;
        disableVrr = true;
        x = 1920;
        primary = true;
      };
      "DP-1" = {
        width = 1920;
        height = 1080;
        disableVrr = true;
      };
      "HDMI-A-1" = {
        x = 4480;
        xorgName = "HDMI-1";
        hyprlandExclude = false;
        edid = ./crt-edited.bin;
        customModes = [
          {
            mode = "512x448@120.00";
            name = "SNES 512x448";
          }
          {
            mode = "640x480@120.01";
            name = "EMU 640x480@120";
          }
          {
            mode = "640x528@120.02";
            name = "GC 640x528@120";
          }
          {
            mode = "480x320@120.00";
            name = "GBA 480x320@120";
          }
          {
            mode = "640x480@144.87";
            name = "640x480@144";
          }
          {
            mode = "800x600@112.51";
            name = "800x600@112";
          }
          {
            mode = "1024x768@90.57";
            name = "1024x768@90";
          }
          {
            mode = "1280x1024@67.02Hz";
            name = "1280x1024@67";
          }
          {
            mode = "960x540@128.00";
            name = "WIDE 960x540@128";
          }
          {
            mode = "1280x720@96.00Hz";
            name = "WIDE 1280x720@96";
          }
        ];
      };
    };
  };

  hardware.keyboard.qmk.enable = true;

  environment.systemPackages = with pkgs; [
    distrobox

    easyeffects
    deepfilternet

    grayjay

    syncthing

    krita

    gpu-screen-recorder-gtk

    qmk
    android-tools

    orca-slicer

    # electrum-ltc
  ];

  hardware.ckb-next.enable = true;

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

  # networking.interfaces."wlp74s0".wakeOnLan = {
  #   enable = true;
  #   policy = ["unicast"];
  # };

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
}
