{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  makeWrapper,
  bun,
}:
let
  inherit (lib) getExe;
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "anki-mcp";
  version = "0.1.0-unstable-2026-01-01";

  src = fetchFromGitHub {
    owner = "obviyus";
    repo = "anki-mcp";
    rev = "d5c0f692f6c12b10521c0b915e57ef544d362305";
    hash = "sha256-mZzvkxXmiyfsefHS8qCtGyx89v93Gim9Fx4Z10qnrmM=";
  };

  # fixed output, bun needs network to resolve the lockfile
  node_modules = stdenvNoCC.mkDerivation {
    pname = "${finalAttrs.pname}-node_modules";
    inherit (finalAttrs) version src;

    nativeBuildInputs = [ bun ];

    dontConfigure = true;
    dontFixup = true;

    buildPhase = ''
      runHook preBuild
      export HOME=$TMPDIR
      bun install --frozen-lockfile --no-progress --ignore-scripts
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      cp -r node_modules $out
      runHook postInstall
    '';

    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = "sha256-dPw+RKetcw6Zv3m/G1SGrlI2Mn69BpQxkGLwfZtImuo=";
  };

  nativeBuildInputs = [ makeWrapper ];
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/anki-mcp $out/bin
    cp -r package.json src $out/share/anki-mcp/
    cp -r ${finalAttrs.node_modules} $out/share/anki-mcp/node_modules
    makeWrapper ${getExe bun} $out/bin/anki-mcp \
      --add-flags "run $out/share/anki-mcp/src/index.ts"
    runHook postInstall
  '';

  meta = {
    description = "Read-only MCP server for Anki via AnkiConnect";
    homepage = "https://github.com/obviyus/anki-mcp";
    mainProgram = "anki-mcp";
    platforms = lib.platforms.linux;
  };
})
