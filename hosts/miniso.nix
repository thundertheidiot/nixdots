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
        disabledModules = ["home/browser/default.nix"];

        config = {
          home.stateVersion = "25.05";
          mHome.browser.firefox.enable = false;
        };
      }
    ];

    boot.loader.timeout = lib.mkForce 10;

    meow = {
      user = "nixos";

      emacs.enable = false;
      shell.enable = true;

      ssh.key = true;
      ssh.rootKey = true;
    };

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  };
}
