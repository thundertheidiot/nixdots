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

  meow = {
    # fullSetup = true;
    workstation.enable = true;
    workstation.environment = ["plasma"];

    user = "thunder";

    # gaming.enable = true;
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

  # services.tlp = {
  #   enable = true;
  #   settings = {
  #     CPU_SCALING_GOVERNOR_ON_AC = "performance";
  #     CPU_SCALING_GOVERNOR_ON_BAT = "ondemand";

  #     CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
  #     CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";

  #     CPU_MIN_PERF_ON_AC = 0;
  #     CPU_MAX_PERF_ON_AC = 100;
  #     CPU_MIN_PERF_ON_BAT = 0;
  #     CPU_MAX_PERF_ON_BAT = 70;

  #     CPU_BOOST_ON_AC = 1;
  #     CPU_BOOST_ON_BAT = 0;

  #     CPU_HWP_DYN_BOOST_ON_AC = 1;
  #     CPU_HWP_DYN_BOOST_ON_BAT = 0;

  #     START_CHARGE_THRESH_BAT0 = 80;
  #     STOP_CHARGE_THRESH_BAT0 = 85;
  #     RESTORE_THRESHOLDS_ON_BAT = 1;
  #   };
  # };

  # users.users."thunder".extraGroups = ["docker"];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/disk/by-id/ata-KINGSTON_SA400S37240G_50026B7380C44F59";
  # boot.loader.grub.useOSProber = true;

  boot.initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
