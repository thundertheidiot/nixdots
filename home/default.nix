{
  pkgs,
  localconfig,
  inputs,
  ...
}: let
  lib = pkgs.lib;
in {
  home = {
    username = localconfig.username;
    homeDirectory = localconfig.homeDirectory;
    stateVersion = localconfig.homeStateVersion;
  };

  scheme = "${inputs.tt-schemes}/base16/dracula.yaml";

  # imports = [
  #   inputs.base16.homeManagerModule
  #   ./base
  #   (lib.mkIf (localconfig.install.gui) { imports = ./gui; })
  #   (lib.mkIf (localconfig.install.gui) { imports = ./emacs; })
  #   (lib.mkIf (localconfig.install.firefox) { imports = ./firefox; })
  #   (lib.mkIf (localconfig.install.awesomewm) { imports = ./awesome; })
  #   (lib.mkIf (localconfig.install.hyprland) ./hyprland)
  # ];

  imports =
    [
      inputs.base16.homeManagerModule
      ./base
    ]
    ++ (
      if localconfig.install.desktop
      then [./desktop ./emacs]
      else []
    )
    ++ (
      if localconfig.install.firefox
      then [./firefox]
      else []
    )
    ++ (
      if localconfig.install.awesomewm
      then [./awesome]
      else []
    )
    ++ (
      if localconfig.install.hyprland
      then [./hyprland]
      else []
    );
}
