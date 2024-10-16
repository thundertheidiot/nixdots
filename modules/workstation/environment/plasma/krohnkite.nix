{
  fetchFromGitHub,
  libsForQt5,
  buildNpmPackage,
  p7zip,
}:
buildNpmPackage {
  pname = "krohnkite";
  version = "0.9.8.1";

  src = fetchFromGitHub {
    owner = "anametologin";
    repo = "krohnkite";
    rev = "5b1446dd0201e34eccc4cb931b80960daefccb7a";
    hash = "sha256-Cy+T2t+e015Y4J42sns4xzltiILErTL4PvcAeF3AJHE=";
  };

  npmDepsHash = "sha256-lsABgLU8B2AIGMvn6nvAFcliVQL8tYYnAxOVh1aTto0=";

  dontConfigure = true;

  buildFlags = ["package"];

  nativeBuildInputs = [libsForQt5.plasma-framework p7zip];
  dontNpmBuild = true;

  dontWrapQtApps = true;

  installPhase = ''
    runHook preInstall

    plasmapkg2 --install pkg --packageroot $out/share/kwin/scripts

    runHook postInstall
  '';
}
