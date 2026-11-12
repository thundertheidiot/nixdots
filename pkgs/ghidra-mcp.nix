{
  lib,
  stdenvNoCC,
  makeWrapper,
  unzip,
  fetchurl,
  python3,
  python3Packages,
  ghidra,
  openjdk25,
}:
let
  # upstream tags are major.minor
  version = "2.0";

  # upstream pins mcp sdk 1.x, nixpkgs only ships 2.x
  mcp_1 = python3Packages.buildPythonPackage {
    pname = "mcp";
    version = "1.30.0";
    format = "wheel";

    # pypi wheel, fetchpypi guesses the wrong tag
    src = fetchurl {
      url = "https://files.pythonhosted.org/packages/f5/f4/e58bc33317c92a0203664daaf00bf6f41166cc0149e5d6870a03f7cd004a/mcp-1.30.0-py3-none-any.whl";
      hash = "sha256-Zm7bUAlQPhBHydYDRqdW+UsmHwXMJiXyPUHHKP/EhNA=";
    };
    dependencies = with python3Packages; [
      anyio
      httpx
      httpx-sse
      jsonschema
      pydantic
      pydantic-settings
      pyjwt
      cryptography
      python-multipart
      sse-starlette
      starlette
      typing-extensions
      typing-inspection
      uvicorn
    ];

    # tests need network services
    doCheck = false;
  };

  release = fetchurl {
    url = "https://github.com/LaurieWired/GhidraMCP/releases/download/${version}/GhidraMCP-release-${
      lib.replaceStrings [ "." ] [ "-" ] version
    }.zip";
    hash = "sha256-6/kE/WeC5CnpWmCiT+BySyDArLLe2zBBNaUoH2tfLnI=";
  };

  # bridge wheel and headless backend jar live in the release zip
  assets = stdenvNoCC.mkDerivation {
    pname = "ghidra-mcp-release";
    inherit version;
    src = release;
    sourceRoot = ".";
    nativeBuildInputs = [ unzip ];

    installPhase = ''
      runHook preInstall
      unzip -q GhidraMCP-release-*/ghidra_*_PUBLIC_*_GhidraMCP.zip
      install -Dm644 GhidraMCP/lib/GhidraMCP.jar $out/share/ghidra-mcp/GhidraMCP.jar
      install -Dm644 -t $out GhidraMCP-release-*/ghidra_mcp-*-py3-none-any.whl
      runHook postInstall
    '';
  };

  ghidra-mcp-py = python3Packages.buildPythonApplication {
    pname = "ghidra-mcp";
    inherit version;
    format = "wheel";
    # wheel version has a patch component the tag lacks
    src = assets;
    unpackPhase = ''
      mkdir -p dist
      cp $src/*.whl dist/
    '';

    propagatedBuildInputs = [
      mcp_1
      python3Packages.pyghidra
      python3Packages.requests
    ];

    meta.mainProgram = "ghidra-mcp";
  };
in
stdenvNoCC.mkDerivation {
  pname = "ghidra-mcp";
  inherit version;

  nativeBuildInputs = [ makeWrapper ];
  dontUnpack = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    makeWrapper ${lib.getExe ghidra-mcp-py} $out/bin/ghidra-mcp \
      --set-default GHIDRA_INSTALL_DIR ${ghidra}/lib/ghidra \
      --set-default JAVA_HOME ${openjdk25.home} \
      --add-flags "--extension-jar ${assets}/share/ghidra-mcp/GhidraMCP.jar"
    # one-shot stdio client for agents
    install -Dm644 ${./ghmcp.py} $out/libexec/ghmcp.py
    makeWrapper ${python3.interpreter} $out/bin/ghmcp \
      --set GHMCP_EXE $out/bin/ghidra-mcp \
      --add-flags $out/libexec/ghmcp.py
    runHook postInstall
  '';

  # catches a broken python env or wrapper without starting a jvm
  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    $out/bin/ghidra-mcp --help > /dev/null
    runHook postInstallCheck
  '';

  passthru = {
    # lets nix-update find the release url
    src = release;
    inherit assets mcp_1;
  };

  meta = {
    description = "MCP server that exposes Ghidra reverse-engineering capabilities to LLM clients";
    homepage = "https://github.com/LaurieWired/GhidraMCP";
    license = lib.licenses.asl20;
    mainProgram = "ghidra-mcp";
    platforms = lib.platforms.linux;
  };
}
