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
    ./opencode.nix
    ./albedo
  ];

  options.mHome.ai.enable = mkEnOpt "Set up ai things";

  config = mkIf cfg {
    home.packages = [ pkgs.python314Packages.trafilatura ];
  };
}
