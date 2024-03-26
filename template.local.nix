{
  username = "user";
  homeDirectory = "/home/user";
  homeStateVersion = "24.05";
  system = "x86_64-linux";
  timeZone = "Europe/Helsinki";
  hostName = "hostname";

  # By default my basic environment is installed, fish shell, emacs, etc.
  install = {
    desktop = true;
	gaming = true;
    firefox = true;
    hyprland = true;
    awesomewm = true;
  };

  hyprland = {
    config = ""; # Extra stuff added to the hyprland config, for monitor setups etc.
    startup = ""; # Extra commands to run on startup, \n separated
  };

  systemConfig = {...}: {
    # Machine specific configuration, filesystems, bootloader, basically hardware-configuration.nix + system.stateVersion
  };
}
