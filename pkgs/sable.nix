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
  version = "1.22.11";

  src = fetchFromGitHub {
    owner = "SableClient";
    repo = "Sable";
    rev = "v${finalAttrs.version}";
    hash = "sha256-RwYKaJ4k2lqXjVSPN/o0Qw8GIGnXfb6acCpva36qQoM=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-sI0tE73l+yKS8186tUHrNAJmGD26XEe7828ohvvc+1o=";
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
