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
