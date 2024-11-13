let
  pins = import ./npins;

  flake = import pins.flake-compat;

  inputs' = flake {src = ./.;};
  inputs = inputs'.defaultNix.inputs;

  pkgs = import pins.nixpkgs {};
  lib = pkgs.lib;

  mlib = import ./lib {inherit lib;};
  mpkgs = import ./pkgs;

  inherit (mlib) mkSystem;
in
  builtins.listToAttrs (builtins.map (s: {
    name = s;
    value = mkSystem {
      nixosSystem = import "${inputs.nixpkgs}/nixos/lib/eval-config.nix";
      # inputs = pins;
      inherit inputs mlib mpkgs;
      config = mlib.getHostConfig s;
    };
  }) ["desktop" "server" "x220" "t440p" "digiboksi"])
