{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  openssl,
  tailwindcss_4,
  dioxus-cli,
  wasm-bindgen-cli_0_2_114,
  lld,
  binaryen,
  nix-update,
  writeShellApplication,
}:
rustPlatform.buildRustPackage {
  pname = "soulbeet";
  version = "0.7.0";

  src = fetchFromGitHub {
    owner = "terry90";
    repo = "soulbeet";
    rev = "49f94d957bb2203a47c8c599caa324aec28e623a";
    hash = "sha256-8JvRRo+QRQ1kjNeBVgXZgEsF0GKaDXQaMKx/oAfyDYA=";
  };
  cargoHash = "sha256-lvlWfknAKaP7niEtI6V+OJgBKhwuMOcL+L4VbbB2WC0=";

  nativeBuildInputs = [
    pkg-config
    tailwindcss_4
    dioxus-cli
    wasm-bindgen-cli_0_2_114
    lld
    binaryen
  ];
  buildInputs = [ openssl ];

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
      # re-evaluate the updated source and refresh dependencies even on retries.
      nix-update --flake --version skip --no-src "$UPDATE_NIX_ATTR_PATH"
    '';
  };
  meta = {
    description = "Music downloader and library manager";
    homepage = "https://github.com/terry90/soulbeet";
    license = lib.licenses.mit;
    mainProgram = "soulbeet";
    platforms = lib.platforms.linux;
  };
}
