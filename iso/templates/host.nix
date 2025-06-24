{...}: let
  cfg = builtins.fromTOML ./meow.toml;

  inherit (cfg) user hostname initialPassword desktopEnvironment;
in {
  imports = [
    ./disko.nix
  ];

  system.stateVersion = "25.05";

  time.timeZone = "Europe/Helsinki";
  networking.hostName = hostname;
  nixpkgs.hostPlatform = "x86_64-linux";

  users.users."${user}".initialPassword = initialPassword;

  meow = {
    workstation.enable = true;
    workstation.environment = [desktopEnvironment];

    impermanence.enable = true;

    shell.enable = true;
    emacs.enable = true;

    ssh.key = false;

    inherit user;

    home.stateVersion = "25.05";
  };

  boot.loader.grub.enable = true;

  boot.initrd.availableKernelModules = ["ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
}
