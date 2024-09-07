pkgs:
pkgs.buildNpmPackage {
  pname = "polonium";
  version = "1.0.0";

  src = pkgs.fetchgit {
    url = "https://github.com/zeroxoneafour/polonium";
    rev = "f17754a76efb3b07a4dbde53e0d2c055271a4882";
    hash = "sha256-GCSL3eg4729uNVZVAFUgCO3i0BiOkFgxQNgXbBCz1XA=";
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
