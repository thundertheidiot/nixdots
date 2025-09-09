{
  lib,
  inputs,
  pkgs,
  ...
}: {
  imports = [
    "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-base.nix"
    "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
  ];

  config = {
    networking.hostName = "meow-iso";
    system.stateVersion = "25.05";

    time.timeZone = "Europe/Helsinki";

    services = {
      qemuGuest.enable = true;
    };

    # networking.wireless.enable = true;

    boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;

    services.displayManager.sddm = {
      settings = {
        Autologin = {
          Session = "hyprland.desktop";
          User = "nixos";
        };
      };
    };

    users.users.nixos = {
      password = "password";
      initialHashedPassword = lib.mkForce null;
    };
    users.extraUsers.root = {
      password = "password";
      initialHashedPassword = lib.mkForce null;
    };

    networking.useDHCP = lib.mkForce true;
    services.openssh.settings.PermitRootLogin = lib.mkForce "prohibit-password";

    home-manager.sharedModules = [
      {
        home.stateVersion = "25.05";
        mHome.browser.firefox.enable = true;
        mHome.emacs.enable = true;
      }
    ];

    meow = {
      user = "nixos";

      workstation.enable = true;
      workstation.environment = ["hyprland"];
      workstation.plasma.opinionatedConfig = true;
      workstation.flatpak.enable = false;

      gaming.enable = false;

      emacs.enable = true;
      shell.enable = true;

      ssh.key = true;
      ssh.rootKey = true;
    };

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  };
}
