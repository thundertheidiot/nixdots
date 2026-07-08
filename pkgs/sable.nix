{
  fetchFromGitHub,
  fetchPnpmDeps,
  nodejs,
  pnpmConfigHook,
  pnpm_10,
  git,
  stdenv,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "sable";
  version = "1.18.3";

  src = fetchFromGitHub {
    owner = "SableClient";
    repo = "Sable";
    rev = "v${finalAttrs.version}";
    hash = "sha256-yi70WBH0lDw1h4Oy6NNfi71kp32be3rtZDt3/C2e524=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-y5Gv/IQ5qkxhj8QHtv7p16bj/f3eHWXGoeZ4CPwkxhY=";
  };

  nativeBuildInputs = [
    nodejs
    pnpmConfigHook
    pnpm_10
    git
  ];

  buildPhase = ''
    pnpm run build
  '';

  installPhase = ''
    cp -r dist $out
  '';
})
