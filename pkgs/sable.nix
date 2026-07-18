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
  version = "1.20.0";

  src = fetchFromGitHub {
    owner = "SableClient";
    repo = "Sable";
    rev = "v${finalAttrs.version}";
    hash = "sha256-ams6Uwblo5mUzah9hlCPCUMXtrLtiUtRWwwKH4uabmk=";
  };

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    pnpm = pnpm_10;
    fetcherVersion = 3;
    hash = "sha256-iBvtMeYUHWhsz1DnKjTzydAt3cGPaisUhaagoaZRg1M=";
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
