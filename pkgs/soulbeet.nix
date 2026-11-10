{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
  sqlite,
  tailwindcss_4,
  dioxus-cli,
  zstd,
  wasm-bindgen-cli_0_2_114,
  lld,
  binaryen,
  bash,
  python3,
  util-linux,
  nix-update,
  writeShellApplication,
}:
rustPlatform.buildRustPackage (finalAttrs: {
  pname = "soulbeet";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "terry90";
    repo = "soulbeet";
    tag = "v${finalAttrs.version}";
    hash = "sha256-cQuaP3YYCFP5Ewpo1kZ5i/TsdD6KMhbHztiZ/AG9Ue0=";
  };
  cargoHash = "sha256-iIvOGHMdl/5dMCDGGrKR5jooJQ9o8nBLf3uqbsrrvDY=";

  nativeBuildInputs = [
    pkg-config
    tailwindcss_4
    (dioxus-cli.overrideAttrs (old: {
      buildInputs = old.buildInputs ++ [ zstd ];
    }))
    wasm-bindgen-cli_0_2_114
    lld
    binaryen
  ];
  buildInputs = [
    openssl
    sqlite
  ];
  nativeCheckInputs = [
    python3
    util-linux
  ];

  postPatch = ''
    substituteInPlace lib/soulbeet/src/beets/probe.rs \
      --replace-fail /bin/bash ${lib.getExe bash}
  '';

  buildPhase = ''
    runHook preBuild
    tailwindcss -i web/assets/input.css -o web/assets/tailwind.css
    dx bundle --package web --release --offline --frozen
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r target/dx/web/release/web/* $out/
    mkdir -p $out/bin
    ln -s ../server $out/bin/soulbeet
    cp beets_config.yaml $out/beets_config.yaml
    cp -r beets_plugins $out/beets-plugins
    substituteInPlace $out/beets_config.yaml \
      --replace-fail 'plugins: musicbrainz' 'plugins: musicbrainz mbtwopass' \
      --replace-fail /opt/beets-plugins "$out/beets-plugins"
    runHook postInstall
  '';

  passthru.autoUpdate.script = writeShellApplication {
    name = "update-soulbeet";
    runtimeInputs = [ nix-update ];
    text = ''
      nix-update --flake --use-github-releases --src-only "$UPDATE_NIX_ATTR_PATH"
      # refresh source and dependencies even on retries
      nix-update --flake --version skip "$UPDATE_NIX_ATTR_PATH"
    '';
  };
  meta = {
    description = "Music downloader and library manager";
    homepage = "https://github.com/terry90/soulbeet";
    license = lib.licenses.mit;
    mainProgram = "soulbeet";
    platforms = lib.platforms.linux;
  };
})
