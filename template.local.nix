{
  username = "thunder";
  homeDirectory = "/home/thunder";
  homeStateVersion = "24.05";
  system = "x86_64-linux";
  timeZone = "Europe/Helsinki";
  hostName = "bruh";

  install = {
    firefox = true;
    desktop = true; # gnome-keyring, notifs, etc. utils
    hyprland = true;
  };

  homeManagerConfig = {localconfig, ...}: {
    home-manager.users.${localconfig.username}.home.stateVersion = "24.05";
  };

  systemConfig = {
    config,
    lib,
    pkgs,
    modulesPath,
    ...
  }: {
  };
}
