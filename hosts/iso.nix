{
  systemArch = "x86_64-linux";

  options = {
    config,
    pkgs,
    ...
  }: {
    username = "iso";
    hostName = "nixos-install";
    timeZone = "Europe/Helsinki";

    workstation.enable = true;
    workstation.utils = "generic/gtk";
    workstation.environment = ["hyprland"];
  };

  system = {
    lib,
    config,
    pkgs,
    ...
  }: {
    system.stateVersion = "25.05";

    services = {
      qemuGuest.enable = true;
    };

    boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;

    services.displayManager.sddm = {
      settings = {
        Autologin = {
          Session = "hyprland.desktop";
          User = "${config.username}";
        };
      };
    };

    users.users.${config.username} = {
      password = "iso";
    };
    users.extraUsers.root.password = "iso";

    networking.useDHCP = lib.mkForce true;
    services.openssh.settings.PermitRootLogin = lib.mkForce "prohibit-password";

    home-manager.sharedModules = [
      {
        stateVersion = "25.05"
        mHome.browser.zen.enable = true;
      }
    ];

    meow = {
      workstation.enable = true;

      emacs.enable = true;
      shell.enable = true;
    };

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  };
}
