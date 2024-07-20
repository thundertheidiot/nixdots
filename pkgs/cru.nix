{
  lib,
  stdenv,
  fetchzip,
  buildFHSEnv,
}:
stdenv.mkDerivation rec {
  name = "custom-resolution-utility";

  src = fetchzip {
    url = "https://www.monitortests.com/download/cru/cru-1.5.2.zip";
    hash = "sha256-UM7Y33NCurzXz9HS4671ll2J/l8xbzHhgUFlCTCIMCY=";
    stripRoot = false;
  };

  installPhase = let
    fhs = buildFHSEnv {
      name = "cru_fhs";
      targetPkgs = pkgs:
        with pkgs; [
          glibc.bin
          wineWowPackages.stagingFull
        ];
    };
  in ''
    mkdir -p $out/bin
    cp CRU.exe $out/CRU.exe
    echo "#!/bin/sh
    ${fhs}/bin/cru_fhs wine $out/CRU.exe" > $out/bin/cru
    chmod +x $out/bin/cru
  '';

  meta = with lib; {
    description = "Custom Resolution Utility (CRU) is an EDID editor that focuses on custom resolutions.";
    homepage = "https://www.monitortests.com/forum/Thread-Custom-Resolution-Utility-CRU";
    license = licenses.mit;
    maintainers = [];
    platforms = with platforms; linux;
  };
}
