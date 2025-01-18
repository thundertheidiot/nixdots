{lib, ...}: let
  inherit (builtins) readDir filter attrNames;
  inherit (lib.attrsets) mergeAttrsList;

  importFilesAndDirs = {
    dir,
    filterF ? (n: n != "default.nix"),
  }:
    map (f: import (dir + "/${f}"))
    (filter filterF (attrNames (readDir dir)));

  imports =
    map (f: f {inherit lib;})
    (importFilesAndDirs {dir = ./.;});

  merged = mergeAttrsList imports;
in
  {
    inherit importFilesAndDirs;
    fun = f: f;
  }
  // merged
