let
  npins = import ./npins/wrapper.nix;

  flake = import npins.flake-compat;

  inputs' = flake {src = ./.;};
  inputs = inputs'.defaultNix.inputs // npins;

  pkgs = import inputs.nixpkgs {};
  lib = pkgs.lib;

  mlib = import ./lib {inherit lib;};
  mpkgs = import ./pkgs;

  inherit (mlib) mkSystem;
in
  builtins.listToAttrs (builtins.map (s: {
    name = s;
    value = mkSystem {
      nixosSystem = import "${inputs.nixpkgs}/nixos/lib/eval-config.nix";
      inherit inputs mlib mpkgs;
      config = mlib.getHostConfig s;

      extraModules = [
        {
          nixpkgs.flake.source = inputs.nixpkgs;
        }
      ];
    };
  }) ["desktop" "server" "x220" "t440p" "digiboksi"])
