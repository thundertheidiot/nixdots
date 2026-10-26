{
  config,
  mlib,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib) getExe mkIf mkMerge;
  inherit (mlib) mkEnOptTrue;

  cfg = config.mHome.ai.playwright-mcp.enable;
  playwrightMcp = pkgs.playwright-mcp;
  helium = pkgs.mpkgs.helium;

  # the playwright mcp bridge extension installed by modules/programs/helium.nix
  extensionId = "mmlmfjhmonkocbjadbfplnigmagldckm";

  tokenPython = pkgs.python3.withPackages (ps: [ ps.plyvel ]);

  # prints the bridge token or nothing if a stable temporary database copy
  # cannot be opened or the stored value is malformed
  readToken = pkgs.writeShellScriptBin "read-pw-token" ''
    exec ${tokenPython}/bin/python ${./read-token.py} "$@"
  '';

  # keep extension mode on the real helium profile, isolated mode bypasses it
  heliumMcp = pkgs.writeShellApplication {
    name = "playwright-mcp";
    runtimeInputs = [
      readToken
      playwrightMcp
      helium
    ];
    text = ''
      export HOME="''${STUBBORN_HOME_DIRECTORY:-$HOME}"
      export PLAYWRIGHT_MCP_USER_DATA_DIR="''${XDG_CONFIG_HOME:-$HOME/.config}/net.imput.helium"
      profile="$PLAYWRIGHT_MCP_USER_DATA_DIR/Default/Local Storage/leveldb"
      token="$(read-pw-token "$profile" ${extensionId} 2>/dev/null || true)"
      if [ "''${#token}" -eq 43 ]; then
        export PLAYWRIGHT_MCP_EXTENSION_TOKEN="$token"
      fi
      exec playwright-mcp --extension --executable-path ${getExe helium} --output-dir "''${XDG_CACHE_HOME:-$HOME/.cache}/playwright-mcp" "$@"
    '';
  };
in
{
  options.mHome.ai.playwright-mcp.enable = mkEnOptTrue "Playwright MCP extension mode for Helium";

  config = mkIf cfg (mkMerge [
    {
      home.packages = [ heliumMcp ];
    }
    {
      mHome.ai.albedo.skills."playwright-mcp/SKILL.md".source = ./playwright-mcp.md;
    }
    {
      mHome.ai.albedo.mcpServers.playwright = {
        type = "stdio";
        command = getExe heliumMcp;
        callTimeoutMs = 120000;
      };
    }
  ]);
}
