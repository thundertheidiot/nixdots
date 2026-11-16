{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf;
  inherit (mlib) mkEnOptTrue;

  cfg = config.mHome.ai.ghidra-mcp.enable;
in
{
  options.mHome.ai.ghidra-mcp.enable = mkEnOptTrue "Ghidra MCP headless tooling and agent skill";

  config = mkIf cfg {
    home.packages = [ pkgs.mpkgs.ghidra-mcp ];

    mHome.ai.skills.ghidra-mcp = ./ghidra-mcp.md;
  };
}
