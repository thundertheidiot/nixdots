{
  lib,
  stdenv,
  fetchzip,
  gnused,
  buildFHSEnv,
  fna3d,
  sdl3,
}:
let
  inherit (lib) getLib;
in
stdenv.mkDerivation (finalAttrs: {
  pname = "duck_game_rebuilt";
  version = "1.4.8.4";

  src = fetchzip {
    url = "https://github.com/TheFlyingFoool/DuckGameRebuilt/releases/download/v${finalAttrs.version}/DuckGameRebuilt.zip";
    hash = "sha256-JgKKULRWjDw4z5xkYQjVocF0X4b/GZMm1oecmuwTlkA=";
    stripRoot = false;
  };

  nativeBuildInputs = [ gnused ];

  installPhase =
    let
      renderer = fna3d.override { useSDL3 = true; };
      fhs = buildFHSEnv {
        name = "dgr_fhs";
        targetPkgs =
          pkgs: with pkgs; [
            glibc.bin
            mono
            sdl3
          ];
      };
    in
    ''
      mkdir -p $out/bin
      cp -r $src $out/DuckGameRebuilt
      chmod 755 $out/DuckGameRebuilt
      ln -sf ${getLib renderer}/lib/libFNA3D.so.0 $out/DuckGameRebuilt/libFNA3D.so.0
      ln -sf ${getLib sdl3}/lib/libSDL3.so.0 $out/DuckGameRebuilt/libSDL3.so.0
      sed 's/ | tee outputlog.txt//g' -i $out/DuckGameRebuilt/DuckGame.sh
      # script
      echo "#!/bin/sh
      cd $out/DuckGameRebuilt
      [ -n \"\$STUBBORN_HOME_DIRECTORY\" ] && export HOME=\"\$STUBBORN_HOME_DIRECTORY\"
      ${fhs}/bin/dgr_fhs ./DuckGame.sh \"\$@\"" > $out/bin/duck_game_rebuilt
      chmod +x $out/bin/duck_game_rebuilt
    '';

  meta = with lib; {
    description = "Duck Game Rebuilt is a decompilation of Duck Game with massive improvements to performance, compatibility, and quality of life features.";
    homepage = "https://github.com/TheFlyingFoool/DuckGameRebuilt/tree/master";
    maintainers = [ ];
    platforms = with platforms; linux;
  };
})
