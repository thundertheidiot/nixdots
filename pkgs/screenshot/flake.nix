{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];
  in
    flake-utils.lib.eachSystem supportedSystems (system: let
      overlays = [
        haskellNix.overlay
        (final: prev: {
          hixProject = final.haskell-nix.hix.project {
            src = ./.;
            evalSystem = "x86_64-linux";
          };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.hixProject.flake {};
    in
      flake
      // {
        legacyPackages = pkgs;

        packages =
          flake.packages
          // {
            default = pkgs.stdenvNoCC.mkDerivation {
              name = "screenshot";
              src = null;
              unpackPhase = "true";

              nativeBuildInputs = with pkgs; [makeWrapper];

              buildPhase = "";

              installPhase = let
                pkg = flake.packages."screenshot:exe:screenshot";
              in ''
                mkdir -p $out/bin
                makeWrapper ${pkg}/bin/screenshot $out/bin/screenshot \
                  --prefix PATH : ${pkgs.lib.makeBinPath (with pkgs; [tofi wl-clipboard grim slurp])}
              '';
            };
          };
      });

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
