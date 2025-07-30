# This module is
{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) bool str;
  inherit (lib) mkIf;

  en = config.meow.base;
in {
  options = {
    meow.base = mkOpt bool true {
      description = "Base setup for every machine, including servers.";
    };

    meow.timeZone = mkOpt str "Europe/Helsinki" {};
    meow.hostName = mkOpt str "meow" {};
  };

  config = mkIf en {
    i18n.defaultLocale = "en_US.UTF-8";

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
