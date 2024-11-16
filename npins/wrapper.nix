# wrapper for flake inputs
let
  npins = import ./default.nix;
  flake = import npins.flake-compat;

  inherit (builtins) mapAttrs elem;

  flakeList = ["home-manager"];
in
  mapAttrs (name: value:
    if elem name flakeList
    then (flake {src = "${value}";}).defaultNix.outputs
    else value)
  npins
