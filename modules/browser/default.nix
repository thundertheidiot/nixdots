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
      enable = mkOpt (listOf (enum ["firefox" "firedragon"])) [] {
        description = "Browsers to install and configure";
      };
    };
  };

  imports = [
    ./internal.nix
  ];

  config = mkMerge [
    (mkIf (elem "firefox" cfg.enable) {
      environment.systemPackages = [pkgs.firefox];

      meow.browser.firefoxConfig.firefox = {
        configPath = ".mozilla/firefox";
      };
    })
    (mkIf (elem "firedragon" cfg.enable) {
      environment.systemPackages = [pkgs.firedragon];

      meow.browser.firefoxConfig.firedragon = {
        configPath = ".firedragon";
      };
    })
  ];
}
