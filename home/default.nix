{ pkgs, localconfig, inputs, ... }: let
  lib = pkgs.lib;
  colortheme = "${inputs.tt-schemes}/base16/dracula.yaml";
in {
  home = {
    username = localconfig.username;
    homeDirectory = localconfig.homeDirectory;
    stateVersion = localconfig.homeStateVersion;
  };

  scheme = colortheme;

  # imports = [
  #   inputs.base16.homeManagerModule
  #   ./base
  #   (lib.mkIf (localconfig.install.gui) { imports = ./gui; })
  #   (lib.mkIf (localconfig.install.gui) { imports = ./emacs; })
  #   (lib.mkIf (localconfig.install.firefox) { imports = ./firefox; })
  #   (lib.mkIf (localconfig.install.awesomewm) { imports = ./awesome; })
  #   (lib.mkIf (localconfig.install.hyprland) ./hyprland)
  # ];

  imports = [
    inputs.base16.homeManagerModule
    ./base
  ]
  ++ (if localconfig.install.gui then [ ./gui ./emacs ] else [])
  ++ (if localconfig.install.firefox then [ ./firefox ] else [])
  # ++ (if localconfig.install.awesomewm then [ (import ./awesome { config = pkgs.config; inherit pkgs inputs; }) ] else [])
  # ++ (if localconfig.install.hyprland then [ (import ./hyprland { config = pkgs.config; inherit pkgs localconfig inputs; })] else []);
  ++ (if localconfig.install.awesomewm then [ ./awesome ] else [])
  ++ (if localconfig.install.hyprland then [ ./hyprland ] else []);

  # specialArgs = [ localconfig inputs ];
}
