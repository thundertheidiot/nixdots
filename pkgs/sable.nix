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
  version = "1.9.3";

  src = fetchFromGitHub {
    owner = "SableClient";
    repo = "Sable";
    rev = "v${finalAttrs.version}";
    hash = "sha256-MMwqQFEW3giu205YqYpPqtQHBPwsrVfn2gOeA3136VA=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-3qlawvy4/+W82Ia9oNDKHiqOw4lEujcrMOVaV5PY0dg=";
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
