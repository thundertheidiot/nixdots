{ lib, pkgs }:
let
  inherit (lib) getExe;
in
{
  id = "thunder/claude-usage";
  description = "Show Claude 5-hour and weekly usage against elapsed time";
  plugin_api = 53;
  widget = [ ./usage.luau ];
  panel = [
    {
      id = "usage-panel";
      file = ./panel.luau;
      width = 340;
      height = "content";
      max_height = 420;
      placement = "attached";
      open_near_click = true;
      dismiss_on_outside_click = false;
    }
  ];
  postRun = ''
    substituteInPlace "$out/usage.luau" \
    --replace-fail @USAGE_COMMAND@ "${getExe pkgs.python3} ${./usage.py}"
  '';
}
