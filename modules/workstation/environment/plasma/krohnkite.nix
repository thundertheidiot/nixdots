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
    rev = "f25caab97f979975e696912cdd65bd83d3f58b6a";
    hash = "sha256-MLVyTVdSgqvSbVtuySr90L0a5hyFKuMhJmnpfI80Hog=";
  };

  npmDepsHash = "sha256-BShnkTKfTa3AYZH6L3mbku6W5sZl0gOj7XhIJ8EDhu0=";

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
