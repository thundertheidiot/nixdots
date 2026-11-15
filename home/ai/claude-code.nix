{
  config,
  mlib,
  lib,
  ...
}:
let
  inherit (lib) mkIf;
  inherit (mlib) mkEnOptTrue;

  cfg = config.mHome.ai.enable && config.mHome.ai.claude-code.enable;
in
{
  options.mHome.ai.claude-code.enable = mkEnOptTrue "Set up claude code";

  config = mkIf cfg {
    programs.claude-code = {
      enable = true;
      enableMcpIntegration = true;
    };
  };
}
