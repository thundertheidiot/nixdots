{
  lib,
  mlib,
  config,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) enum;
in {
  options = {
    meow.rice = mkOpt (enum ["glass" "minimal" "none"]) "glass" {
      description = "Theme to install";
    };
  };

  imports = [
    ./glass
    ./minimal
  ];
}
