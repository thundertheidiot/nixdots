{
  lib,
  stdenvNoCC,
  stdenv,
  autoPatchelfHook,
  fetchFromGitHub,
  fetchPnpmDeps,
  nix-update,
  writeShellApplication,
  pnpm_12,
  pnpmConfigHook,
  makeWrapper,
  python3,
  ffmpeg,
  deno,
  aria2,
}:
let
  python = python3.withPackages (
    p: with p; [
      aiohttp
      python-socketio
      yt-dlp
      watchfiles
      mutagen
      curl-cffi
    ]
  );
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "metube";
  version = "2026.11.10";
  src = fetchFromGitHub {
    owner = "alexta69";
    repo = "metube";
    tag = finalAttrs.version;
    hash = "sha256-fIy/pSlU4lKS+KoXmBJTwMwRTCTy16CIN2cRpIknXYw=";
  };
  passthru.autoUpdate.script = writeShellApplication {
    name = "update-metube";
    runtimeInputs = [ nix-update ];
    text = ''
      nix-update --flake --use-github-releases --src-only "$UPDATE_NIX_ATTR_PATH"
      # refresh source and dependencies even on retries
      nix-update --flake --version skip "$UPDATE_NIX_ATTR_PATH"
    '';
  };
  sourceRoot = "source/ui";
  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs)
      pname
      version
      src
      sourceRoot
      ;
    pnpm = pnpm_12;
    fetcherVersion = 5;
    hash = "sha256-3ccw2QrWiYXFkQyCTOn8mFT6o5t/9msU2qOkf5z/2FM=";
  };
  meta = {
    description = "Self-hosted web GUI for yt-dlp";
    homepage = "https://github.com/alexta69/metube";
    license = lib.licenses.agpl3Only;
    maintainers = [ ];
    platforms = [ "x86_64-linux" ];
    mainProgram = "metube";
  };
  nativeBuildInputs = [
    pnpm_12
    pnpm_12.nodejs-slim
    pnpmConfigHook
    makeWrapper
    autoPatchelfHook
  ];
  buildInputs = [ stdenv.cc.cc.lib ];
  buildPhase = ''
    runHook preBuild
    autoPatchelf node_modules
    pnpm run build
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/metube/ui/dist $out/bin
    cp -r ../app $out/share/metube/app
    cp -r dist/metube $out/share/metube/ui/dist/
    makeWrapper ${python}/bin/python $out/bin/metube \
      --add-flags "$out/share/metube/app/main.py" \
      --set BASE_DIR "$out/share/metube" \
      --prefix PATH : ${
        lib.makeBinPath [
          ffmpeg
          deno
          aria2
        ]
      }
    runHook postInstall
  '';
})
