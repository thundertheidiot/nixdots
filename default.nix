# flakeless build support
# provides almost zero benefits for now, eval might be slightly faster, not sure
# nix-build --argstr HOSTNAME desktop --attr config.system.build.toplevel
{host}: let
  flake-compat = import (builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/refs/tags/v1.0.1.tar.gz");

  inputs' = flake-compat {src = ./.;};
  inputs = inputs'.defaultNix.inputs;

  pkgs = import inputs.nixpkgs {};
  lib = pkgs.lib;

  mlib = import ./lib {inherit lib;};

  inherit (mlib) mkSystem;
in
  mkSystem {
    nixosSystem = import "${inputs.nixpkgs}/nixos/lib/eval-config.nix";
    inherit inputs mlib;
    config = mlib.getHostConfig host;

    extraModules = [
      {
        nixpkgs.flake.source = inputs.nixpkgs;
      }
    ];
  }
