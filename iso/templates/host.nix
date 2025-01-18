{...}: let
  hostname = "hostname";
  user = "meow";
  password = "meowmeow"; # Initial password, change later
in {
  imports = [
    ./disko.nix
  ];

  system.stateVersion = "24.11";
  time.timeZone = "Europe/Helsinki";
  networking.hostName = hostname;
  nixpkgs.hostPlatform = "x86_64-linux";

  users.users."${user}".initialPassword = password;

  meow = {
    workstation.enable = true;
    workstation.environment = ["gnome"];
    impermanence.enable = true;

    shell.enable = true;
    emacs.enable = true;

    ssh.key = false;

    inherit user;

    home = {
      stateVersion = "24.11";
    };
  };

  boot.loader.grub.enable = true;

  boot.initrd.availableKernelModules = ["ehci_pci" "ahci" "usb_storage" "sd_mod" "sdhci_pci"];
  boot.initrd.kernelModules = [];
  boot.kernelModules = ["kvm-intel"];
  boot.extraModulePackages = [];
}
