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
  version = "1.13.0";

  src = fetchFromGitHub {
    owner = "SableClient";
    repo = "Sable";
    rev = "v${finalAttrs.version}";
    hash = "sha256-PggtBXvCX7KFZPqSwPPd9UEjjkBAZZucr6Nmn0DUHuo=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-DO8/wCnQY5Wxr4qRiflMKbCKR16Hy7jswQouN3xMYXw=";
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
