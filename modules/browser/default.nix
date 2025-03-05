{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib) mkIf mkMerge mkDefault;
  inherit (lib.types) listOf enum;
  inherit (builtins) elem;

  cfg = config.meow.browser;
in {
  options = {
    meow.browser = {
      enable = mkOpt (listOf (enum ["firefox" "firedragon" "librewolf"])) [] {
        description = "Browsers to install and configure";
      };
    };
  };

  imports = [
    ./internal.nix
  ];

  config = mkMerge [
    (mkIf (elem "firefox" cfg.enable) {
      meow.browser.firefoxConfig.firefox = {
        configPath = ".mozilla/firefox";
        package = pkgs.firefox;
      };
    })
    (mkIf (elem "firedragon" cfg.enable) {
      meow.browser.firefoxConfig.firedragon = {
        configPath = ".firedragon";
        package = pkgs.firedragon;
      };
    })
  ];
}
