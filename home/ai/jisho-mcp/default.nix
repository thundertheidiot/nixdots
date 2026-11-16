{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe mkIf;
  inherit (mlib) mkEnOptTrue;

  cfg = config.mHome.ai.jisho-mcp.enable;
in
{
  options.mHome.ai.jisho-mcp.enable = mkEnOptTrue "Jisho.org japanese dictionary MCP server";

  config = mkIf cfg {
    programs.mcp.servers.jisho.command = getExe pkgs.mpkgs.jisho-mcp;
  };
}
