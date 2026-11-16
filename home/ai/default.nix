{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf;
  inherit (mlib) mkEnOpt;

  cfg = config.mHome.ai.enable;
in
{
  imports = [
    ./skills.nix
    ./opencode.nix
    ./claude-code.nix
    ./playwright-mcp
    ./ghidra-mcp
    ./anki-mcp
    ./albedo
  ];

  options.mHome.ai.enable = mkEnOpt "Set up ai things";

  config = mkIf cfg {
    programs.mcp.enable = true;

    home.packages = [
      pkgs.python314Packages.trafilatura
    ];
  };
}
