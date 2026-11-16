{
  lib,
  python3Packages,
  fetchFromGitHub,
}:
python3Packages.buildPythonApplication {
  pname = "jisho-mcp";
  version = "0-unstable-2026-05-05";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "giuliocapecchi";
    repo = "jisho-mcp";
    rev = "f33af2066231fd6fa350d378a353829877e1b11f";
    hash = "sha256-vmOVfVpFCMnI7QGKEQgUKdFyLOI37oMp2OeXeMIojHU=";
  };

  # fastmcp is broken against mcp 2.x in nixpkgs, use the sdk's renamed builtin instead
  postPatch = ''
    substituteInPlace src/jisho_mcp/server.py \
      --replace-fail "from fastmcp import FastMCP" "from mcp.server import MCPServer as FastMCP"
    substituteInPlace pyproject.toml \
      --replace-fail '"fastmcp>=2.0"' '"mcp>=2.0"'
  '';

  build-system = [ python3Packages.hatchling ];

  dependencies = with python3Packages; [
    mcp
    httpx
    pydantic
  ];

  pythonImportsCheck = [ "jisho_mcp" ];

  # no releases upstream, track the default branch
  passthru.autoUpdate.args = [ "--version=branch" ];

  meta = {
    description = "MCP server for the Jisho.org Japanese dictionary";
    homepage = "https://github.com/giuliocapecchi/jisho-mcp";
    mainProgram = "jisho-mcp";
    platforms = lib.platforms.linux;
  };
}
