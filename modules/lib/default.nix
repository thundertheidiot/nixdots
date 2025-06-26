{lib, ...}: let
  inherit (lib) mkOption;
  inherit (lib.types) attrsOf anything;
in {
  options = {
    flake.lib = mkOption {
      type = attrsOf anything;
      default = {};
    };
  };
}
