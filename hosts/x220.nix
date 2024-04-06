{
  systemArch = "x86_64-linux";
  username = "thunder";
  homeDirectory = "/home/thunder";

  options = {
    config,
    pkgs,
    ...
  }: {
    config = {
      username = "thunder";
      hostName = "x220";
      timeZone = "Europe/Helsinki";

      setup.userMachine.enable = true;
      setup.hyprland.enable = true;
      setup.hyprland.extraConfig = ''
        monitor=LVDS-1,1366x768@60,0x0,1
      '';
      setup.hyprland.extraAutostart = [];
      setup.awesomeWM.enable = true;
      setup.firefox.enable = true;
      setup.gaming.enable = false;
      setup.tv.enable = true;
      setup.laptop.enable = true;

      monitors = [
        { name = "LVDS-1"; width = 1366; height = 768; }
      ];

      setup.gpu = "intel";
    };
  };

  home = {...}: {
    home.stateVersion = "24.05";
  };

  system = {
    config,
    lib,
    pkgs,
    modulesPath,
    ...
  }: {
    system.stateVersion = "23.05";

    boot.loader.grub.enable = true;
    boot.loader.grub.device = "/dev/sda";

    boot.initrd.availableKernelModules = ["ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci"];
    boot.initrd.kernelModules = [];
    boot.kernelModules = ["kvm-intel"];
    boot.extraModulePackages = [];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/d53a7434-16f0-4193-b057-4286168aea61";
      fsType = "btrfs";
    };

    swapDevices = [];

    networking.useDHCP = lib.mkDefault true;

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
