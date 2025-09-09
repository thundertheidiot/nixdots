{lib, ...}: let
  inherit (lib) mkOption;
  inherit (lib.types) attrsOf anything;
in {
  imports = [
    ./modules.nix
  ];

  options = {
    flake.lib = mkOption {
      type = attrsOf anything;
      default = {};
    };
  };
}
