{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf;
  inherit (builtins) readFile;
  inherit (mlib) mkEnOptTrue;

  cfg = config.mHome.ai.ghidra-mcp.enable;
in
{
  options.mHome.ai.ghidra-mcp.enable = mkEnOptTrue "Ghidra MCP headless tooling and agent skill";

  config = mkIf cfg {
    home.packages = [ pkgs.mpkgs.ghidra-mcp ];

    programs.albedo.skills.ghidra-mcp = pkgs.writeTextDir "SKILL.md" (readFile ./ghidra-mcp.md);
  };
}
