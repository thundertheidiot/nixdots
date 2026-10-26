{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) mkIf mkMerge;
  inherit (mlib) mkEnOptTrue;

  cfg = config.mHome.ai.firefox-mcp.enable;

  firefoxDevtoolsMcp = pkgs.mpkgs.firefox-devtools-mcp;

  # own firefox instance and profile for agents
  firefoxMcp = pkgs.writeShellScriptBin "firefox-devtools-mcp" ''
    export PATH="${pkgs.geckodriver}/bin:$PATH"
    export HOME="''${STUBBORN_HOME_DIRECTORY:-$HOME}"
    # real firefox binary, not the wrapper script
    exec ${firefoxDevtoolsMcp}/bin/firefox-devtools-mcp \
      --firefox-path ${config.programs.firefox.finalPackage}/bin/.firefox-wrapped \
      --profile-path ${config.xdg.dataHome}/firefox-devtools-mcp
  '';

  # open the agent profile yourself; not while an agent is driving
  aiFirefox = pkgs.writeShellScriptBin "ai-firefox" ''
    profile="${config.xdg.dataHome}/firefox-devtools-mcp/firefox_devtools_mcp_profile"
    mkdir -p "$profile"
    # always automatable
    exec ${config.programs.firefox.finalPackage}/bin/firefox --profile "$profile" \
      --marionette --remote-debugging-port "$@"
  '';

  # attach to an already-running ai-firefox window
  firefoxMcpAttach = pkgs.writeShellScriptBin "firefox-devtools-mcp-attach" ''
    export PATH="${pkgs.geckodriver}/bin:$PATH"
    export HOME="''${STUBBORN_HOME_DIRECTORY:-$HOME}"
    exec ${firefoxDevtoolsMcp}/bin/firefox-devtools-mcp \
      --connect-existing \
      --marionette-port 2828
  '';
in
{
  options.mHome.ai.firefox-mcp.enable = mkEnOptTrue "Firefox devtools MCP for agents";

  config = mkIf cfg (mkMerge [
    {
      home.packages = [
        firefoxMcp
        firefoxMcpAttach
        aiFirefox
      ];

      home.file.".agents/skills/firefox-mcp/SKILL.md".source = ./firefox-mcp.md;
    }

    # register with albedo
    {
      mHome.ai.albedo.mcpServers = {
        firefox-devtools = {
          type = "stdio";
          command = "${firefoxMcp}/bin/firefox-devtools-mcp";
          # firefox startup is slow
          callTimeoutMs = 120000;
        };
        firefox-devtools-attach = {
          type = "stdio";
          # needs an ai-firefox window open
          command = "${firefoxMcpAttach}/bin/firefox-devtools-mcp-attach";
          callTimeoutMs = 120000;
        };
      };
    }
  ]);
}
