{lib, ...}: let
  inherit (builtins) readDir attrNames;
  inherit (lib) mergeAttrsList pipe;
in {
  # TODO replace with pipe operator when convenient
  flake.lib = pipe null [
    (_: readDir ./.)
    attrNames
    (map (n: import (./. + "/${n}") {inherit lib;}))
    mergeAttrsList
  ];
}
