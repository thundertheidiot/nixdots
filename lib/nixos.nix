{...}: {
  mkSystem = {
    pkgs,
    patches,
    system,
    specialArgs,
    modules,
  }:
    pkgs.lib.nixosSystem {
      inherit system specialArgs modules;
      pkgs = let
        applyPatches = (import pkgs {inherit system;}).appyPatches;
      in
        import (applyPatches {
          name = "pkgs";
          src = "${pkgs}";
          inherit patches;
        }) {inherit system;};
    };

  defaultModules = list: map (m: m.nixosModules.default) list;
}
