# This module is
{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) bool;
  inherit (lib) mkIf;

  en = config.meow.base;
in {
  options = {
    meow.base = mkOpt bool true {
      description = "Base setup for every machine, including servers.";
    };
  };

  config = mkIf en {
    environment.systemPackages = with pkgs; [
      git
      wget
      curl

      fd
      ripgrep
      ncdu
      killall
      which

      btop
      neovim
    ];
  };
}
