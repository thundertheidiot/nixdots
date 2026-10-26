{
  runCommand,
  makeWrapper,
  python314Packages,
}:
let
  pyEnv = python314Packages.python.withPackages (ps: [ ps.trafilatura ]);
in
runCommand "albedo-web-mcp"
  {
    nativeBuildInputs = [ makeWrapper ];
    meta.description = "MCP stdio server for albedo: local SearXNG search + trafilatura fetch";
  }
  ''
    install -Dm644 ${./albedo_web_mcp.py} $out/lib/albedo_web_mcp.py
    makeWrapper ${pyEnv}/bin/python $out/bin/albedo-web-mcp \
      --add-flags $out/lib/albedo_web_mcp.py
  ''
