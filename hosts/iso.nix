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
    system.stateVersion = "24.11";

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

    meow = {
      workstation.enable = true;

      firefox.enable = true;
      emacs.enable = true;
      shell.enable = true;

      home = {
        stateVersion = "24.05";
        modules = [
          ({...}: {
            gtk.gtk3.bookmarks = [
              "file:///mnt/4tb"
            ];
          })
        ];
      };
    };
  };
}
