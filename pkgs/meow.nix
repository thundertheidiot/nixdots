{
  lib,
  haskellPackages,
  stdenv,
  nix-output-monitor,
}:
stdenv.mkDerivation {
  name = "meow";
  version = "0.1";

  src = lib.sourceFilesBySuffices ../meow [".hs"];

  buildInputs = [
    (haskellPackages.ghcWithPackages (p:
      with p; [
        aeson
      ]))
  ];

  propagatedNativeBuildInputs = [
    nix-output-monitor
  ];

  buildPhase = ''
    mkdir -p $out/bin

    ghc $src/*.hs -outputdir ./ -hidir ./ -odir ./ -dumpdir ./ -tmpdir ./ \
      -o $out/bin/meow
  '';
}
