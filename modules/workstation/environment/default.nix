{
  lib,
  mlib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) enum listOf;
in {
  options = {
    meow.workstation.environment = mkOpt (listOf (enum ["hyprland" "plasma" "cosmic"])) ["hyprland"] {
      description = "The list of environments to configure and install.";
    };
  };
  imports = [
    ./hyprland
  ];
}
