{
  lib,
  stdenv,
  fetchzip,
  gnused,
  buildFHSEnv,
  homeDirectory ? null, # DGR Creates a useless $HOME/DuckGame
}:
stdenv.mkDerivation {
  name = "duck_game_rebuilt";

  src = fetchzip {
    url = "https://github.com/TheFlyingFoool/DuckGameRebuilt/releases/download/v1.4.2.1/DuckGameRebuilt.zip";
    hash = "sha256-f+wruk3UujJhx9LxW6oremtqAwBzwrzdSOIEf7rjADU=";
    stripRoot = false;
  };

  nativeBuildInputs = [gnused];

  installPhase = let
    fhs = buildFHSEnv {
      name = "dgr_fhs";
      targetPkgs = pkgs:
        with pkgs; [
          glibc.bin
          mono
          SDL2
          gnome2.gtk
        ];
    };
  in ''
    mkdir -p $out/bin
    cp -r $src $out/DuckGameRebuilt
    chmod 755 $out/DuckGameRebuilt # the below sed operation doesn't work otherwise
    sed 's/ | tee outputlog.txt//g' -i $out/DuckGameRebuilt/DuckGame.sh
    # script
    echo "#!/bin/sh
    cd $out/DuckGameRebuilt
    export HOME=$STUBBORN_HOME
    ${
      if builtins.isString homeDirectory
      then "export HOME=${homeDirectory}"
      else ""
    }
    ${fhs}/bin/dgr_fhs ./DuckGame.sh \$@" > $out/bin/duck_game_rebuilt
    chmod +x $out/bin/duck_game_rebuilt
  '';

  meta = with lib; {
    description = "Duck Game Rebuilt is a decompilation of Duck Game with massive improvements to performance, compatibility, and quality of life features.";
    homepage = "https://github.com/TheFlyingFoool/DuckGameRebuilt/tree/master";
    maintainers = [];
    platforms = with platforms; linux;
  };
}
