{
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage rec {
  pname = "firefox-devtools-mcp";
  version = "0.10.6";

  src = fetchFromGitHub {
    owner = "mozilla";
    repo = "firefox-devtools-mcp";
    rev = "v${version}";
    hash = "sha256-Z3cxGgJia1QrYewLGjmVR1f6daZNI+K4qGFg3vYIN6M=";
  };

  npmDepsHash = "sha256-x6MnmMZPDQtBC2p7oL5p9yWlI9CkvvzoPB323OMCzv0=";
}
