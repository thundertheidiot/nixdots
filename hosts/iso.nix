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

    
  };

  system = {
    lib,
    config,
    ...
  }: {
    system.stateVersion = "24.05";

    services = {
      qemuGuest.enable = true;
    };

    users.users.${config.username} = {
      password = "iso";
    };
    users.extraUsers.root.password = "iso";

    networking.useDHCP = lib.mkForce true;
    services.openssh.settings.PermitRootLogin = lib.mkForce "prohibit-password";

    meow = {
      firefox.enable = true;
      emacs.enable = true;
      shell.enable = true;

      home = {
        stateVersion = "24.05";
        modules = [({...}: {
          gtk.gtk3.bookmarks = [
            "file:///mnt/4tb"
          ];
        })];
      };
    };
  };
}
