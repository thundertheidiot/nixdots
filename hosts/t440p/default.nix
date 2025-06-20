{
  config,
  lib,
  pkgs,
  modulesPath,
  mlib,
  ...
}: {
  system.stateVersion = "25.05";

  time.timeZone = "Europe/Helsinki";
  networking.hostName = "t440p";

  # virtualisation.docker.enable = true;

  imports = [
    # ./desktop/firedragon.nix
    ./disko.nix
  ];

  users.users.thunder.initialPassword = "password";

  meow = {
    # fullSetup = true;
    workstation.enable = true;
    workstation.environment = ["plasma"];

    user = "thunder";

    gaming.enable = true;
    # gaming.emulation = true;
    # gaming.games = ["duckgame"];

    browser.zen.enable = true;

    emacs.enable = true;
    shell.enable = true;

    ssh.rootKey = true;

    gpu = "intel";

    keyboard = {
      enable = true;
      devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
    };

    monitors = [
      {
        name = "eDP-1";
        width = "1920";
        height = "1080";
      }
    ];

    home = {
      stateVersion = "25.05";
    };
  };

  # users.users."thunder".extraGroups = ["docker"];

  boot.loader.grub.enable = true;

  boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
