{stdenvNoCC}:
stdenvNoCC.mkDerivation {
  name = "kodi-firefox-launcher";

  dontStrip = true;

  namescape = "script.firefox.launcher";
  version = "1.0.0";

  extraRuntimeDependencies = [];

  src = ./.;

  installPhase = ''
    runHook preInstall

    d=$out/share/kodi/addons
    mkdir -p $d

    cp -R ./* $d

    runHook postInstall
  '';
}
