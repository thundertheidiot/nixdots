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

  cfg = config.mHome.ai.anki-mcp.enable;
in
{
  options.mHome.ai.anki-mcp.enable =
    mkEnOptTrue "Anki MCP server, requires anki with ankiconnect running";

  config = mkIf cfg {
    programs.mcp.servers.anki.command = getExe pkgs.mpkgs.anki-mcp;

    mHome.ai.skills.anki-sentences = ./sentences.md;
  };
}
