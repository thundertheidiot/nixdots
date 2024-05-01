pkgs:
pkgs.buildNpmPackage {
  pname = "polonium";
  version = "1.0.0";

  src = pkgs.fetchgit {
    url = "https://github.com/zeroxoneafour/polonium";
    rev = "59f232475cd1ce9453657b5c2cff63fc4b911c3b";
    hash = "sha256-65w/eyD4xIOLziK+Y6Mvg2RQLfQZIt/jbWyR63BSUiI=";
  };

  npmDepsHash = "sha256-kaT3Uyq+/JkmebakG9xQuR4Kjo7vk6BzI1/LffOj/eo=";

  dontConfigure = true;

  buildFlags = ["res" "src"];

  nativeBuildInputs = [pkgs.libsForQt5.plasma-framework];
  dontNpmBuild = true;

  dontWrapQtApps = true;

  installPhase = ''
    runHook preInstall

    plasmapkg2 --install pkg --packageroot $out/share/kwin/scripts

    runHook postInstall
  '';
}
