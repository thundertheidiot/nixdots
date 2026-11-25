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
  # copy playwright out of playwright-test, its wrapper pins every bundled browser
  playwrightMcp = pkgs.playwright-mcp.overrideAttrs {
    postInstall = ''
      pkg_dir="$out/lib/node_modules/@playwright/mcp"
      rm -rf "$pkg_dir/node_modules/playwright" "$pkg_dir/node_modules/playwright-core"
      cp -r ${pkgs.playwright-test}/lib/node_modules/{playwright,playwright-core} "$pkg_dir/node_modules/"
      wrapProgram $out/bin/playwright-mcp --set-default PLAYWRIGHT_MCP_BROWSER chromium
    '';
  };
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
      mHome.ai.skills.playwright-mcp = ./playwright-mcp.md;
    }
    {
      programs.mcp.servers.playwright = {
        command = getExe heliumMcp;
        callTimeoutMs = 120000;
      };
    }
    # inline server gives every spawned agent its own connection and tab group
    {
      programs.claude-code.agents.browser = ''
        ---
        name: browser
        description: Web worker with its own private Playwright connection and tab group in the user's Helium. Use for browser tasks instead of the shared playwright tools, and always when several agents browse in parallel.
        mcpServers:
          browser:
            type: stdio
            command: ${getExe heliumMcp}
        disallowedTools: mcp__plugin_hm_playwright
        skills:
          - playwright-mcp
        ---

        You drive Helium through your own `browser` MCP server. Its connection and tab group belong to you alone; other agents may be working in other groups of the same window at the same time.

        - Your first `browser_navigate` reuses your connect tab; stay in it and navigate rather than opening tabs.
        - Only touch tabs listed by your own `browser_tabs`; never close user tabs.
        - Close tabs you opened when done, unless the task is to leave a page up for the user.
        - Report findings concisely; include URLs you used.
      '';
    }
  ]);
}
