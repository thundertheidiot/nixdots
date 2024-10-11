{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) homeModule mkOpt mkEnOpt;
  inherit (lib) mkIf;
  inherit (lib.types) listOf enum;

  cfg = config.meow.browser;
in {
  options = {
    meow.browser = {
      enable = mkOpt (listOf (enum ["firefox" "firedragon"])) ["firedragon"] {
        description = "Browsers to install and configure";
      };
    };
  };

  imports = [
    ./internal.nix
  ];
}
