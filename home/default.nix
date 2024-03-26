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

  scheme = "${inputs.tt-schemes}/base16/catppuccin-mocha.yaml";

  imports = [
    inputs.base16.homeManagerModule
    ./base
    ./desktop
    ./emacs
    ./firefox
    ./awesome
    ./hyprland
    ];
}
