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

  cfg = config.mHome.ai.nixos-mcp.enable;
in
{
  options.mHome.ai.nixos-mcp.enable =
    mkEnOptTrue "NixOS MCP server for nixpkgs, nixos and home-manager options";

  config = mkIf cfg {
    programs.mcp.servers.nixos.command = getExe pkgs.mcp-nixos;
  };
}
