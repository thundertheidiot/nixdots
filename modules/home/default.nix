{
  pkgs,
  inputs,
  config,
  ...
}: let
  lib = pkgs.lib;
in {
  home = {
    username = config.username;
    homeDirectory = config.homeDirectory;
  };

  scheme = "${inputs.tt-schemes}/base16/catppuccin-mocha.yaml";

  imports = [
    # inputs.base16.homeManagerModule
    ./base
    (import ../workstation).home
    (import ../tv).home
    ./gaming
    ./emacs
    ./firefox
    ./tv
  ];
}
