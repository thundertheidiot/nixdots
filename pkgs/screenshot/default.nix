# {fetchFromGitHub}: let
#   haskellNix = import (fetchFromGitHub {
#     owner = "input-output-hk";
#     repo = "haskell.nix";
#     rev = "91fb513e4137d2a763f2227ea4d1b4e3d4225127";
#     hash = "sha256-cwmZpMzLuSOUcPEO8NB/F8iR4xD8WOB08MPYNnd1P3w=";
#   }) {};
#   pkgs =
#     import
#     haskellNix.sources.nixpkgs-unstable
#     haskellNix.nixpkgsArgs;
# in
#   pkgs.haskell-nix.project {
#     src = ./.;
#     compiler-nix-name = "ghc928";
#   }
{
  stdenvNoCC,
  lib,
  haskellPackages,
  makeWrapper,
  tofi,
  wl-clipboard,
  grim,
  slurp,
}:
stdenvNoCC.mkDerivation {
  name = "screenshot";
  src = null;
  unpackPhase = "true";

  nativeBuildInputs = [makeWrapper];

  buildPhase = "";

  installPhase = let
    pkg = haskellPackages.developPackage {
      root = ./.;
    };
  in ''
    mkdir -p $out/bin
    makeWrapper ${pkg}/bin/screenshot $out/bin/screenshot \
      --prefix PATH : ${lib.makeBinPath [tofi wl-clipboard grim slurp]}
  '';
}
