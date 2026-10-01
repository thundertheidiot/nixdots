{
  lib,
  stdenv,
  fetchzip,
  gnused,
  buildFHSEnv,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "duck_game_rebuilt";
  version = "1.4.7";

  src = fetchzip {
    url = "https://github.com/TheFlyingFoool/DuckGameRebuilt/releases/download/v${finalAttrs.version}/DuckGameRebuilt.zip";
    hash = "sha256-6LtCAGh4iqRKUZLDNmsI/HjQCC9Iof0wZEPQQGAO38A=";
    stripRoot = false;
  };

  nativeBuildInputs = [ gnused ];

  installPhase =
    let
      fhs = buildFHSEnv {
        name = "dgr_fhs";
        targetPkgs =
          pkgs: with pkgs; [
            glibc.bin
            mono
            SDL2
            gtk2
          ];
      };
    in
    ''
      mkdir -p $out/bin
      cp -r $src $out/DuckGameRebuilt
      chmod 755 $out/DuckGameRebuilt # the below sed operation doesn't work otherwise
      sed 's/ | tee outputlog.txt//g' -i $out/DuckGameRebuilt/DuckGame.sh
      # script
      echo "#!/bin/sh
      cd $out/DuckGameRebuilt
      [ ! -z $STUBBORN_HOME_DIRECTORY ] && export HOME=$STUBBORN_HOME_DIRECTORY
      ${fhs}/bin/dgr_fhs ./DuckGame.sh \$@" > $out/bin/duck_game_rebuilt
      chmod +x $out/bin/duck_game_rebuilt
    '';

  meta = with lib; {
    description = "Duck Game Rebuilt is a decompilation of Duck Game with massive improvements to performance, compatibility, and quality of life features.";
    homepage = "https://github.com/TheFlyingFoool/DuckGameRebuilt/tree/master";
    maintainers = [ ];
    platforms = with platforms; linux;
  };
})
