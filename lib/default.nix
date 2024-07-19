{lib, ...}: let
  inherit (builtins) readDir filter attrNames;
  inherit (lib.attrsets) mergeAttrsList;

  files = let
    f = readDir ./.;
  in
    filter (s: s != "" && s != ./. + "/default.nix") # filter out self and dirs
    
    (map (
      name:
        if (f.${name} == "regular")
        then ./. + "/${name}"
        else ""
    ) (attrNames f));

  imports =
    map (
      f:
        (import f)
        {inherit lib;}
    )
    files;

  merged = mergeAttrsList imports;
in
  merged
