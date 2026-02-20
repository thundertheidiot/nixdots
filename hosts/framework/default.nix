{
  config,
  lib,
  pkgs,
  modulesPath,
  mlib,
  inputs,
  ...
}: {
  imports = [
    ./disko.nix
    inputs.nixos-hardware.nixosModules.framework-13-7040-amd
    ./kmod.nix # nixos-hardware workaround
    inputs.autoaspm.nixosModules.default
  ];

  system.stateVersion = "25.05";

  time.timeZone = "Europe/Helsinki";
  networking.hostName = "framework";

  # networking.wireless.enable = false;
  # networking.networkmanager.wifi.backend = "iwd";

  users.users.thunder = {
    initialPassword = "password";
    extraGroups = ["docker"];
  };

  powerManagement.enable = true;
  powerManagement.powertop.enable = true;

  services.power-profiles-daemon.enable = true;
  boot.kernelParams = ["amd_pstate=active"];
  services.autoaspm.enable = true;

  services.fprintd = {
    enable = true;
  };

  boot.kernelPackages = pkgs.cachyosKernels.linuxPackages-cachyos-latest-lto-zen4;

  virtualisation.docker.enable = true;

  home-manager.sharedModules = [
    {
      home.stateVersion = "25.05";
      mHome.browser.firefox.enable = true;
      mHome.setup.fullLanguages = true;

      wayland.windowManager.hyprland.settings.xwayland.force_zero_scaling = true;

      home.packages = with pkgs; [
        distrobox

        python313Packages.python

        # electrum-ltc
        # monero-gui
      ];

      services.syncthing = {
        enable = true;
      };
    }
  ];

  services.cpupower-gui.enable = true;

  # boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;

  meow = {
    fullSetup = true;
    workstation.enable = true;
    workstation.environment = ["niri"];
    workstation.displayManager = "gdm";

    impermanence.enable = true;
    impermanence.directories = [
      "/var/lib/fwupd"
      "/var/cache/fwupd"
    ];

    user = "thunder";

    gaming.enable = true;

    emacs.enable = true;
    shell.enable = true;

    school.enable = true;

    ssh.rootKey = true;

    gpu = "intel";

    virtualization.enable = true;

    keyboard = {
      enable = true;
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
    };

    boot.efi = true;
    # displaylink = true;

    monitors."eDP-1" = {
      width = 2880;
      height = 1920;
      refresh = 120.0;
      primary = true;
      scale = 1.5;
    };
  };

  boot.loader.grub.enable = true;

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
