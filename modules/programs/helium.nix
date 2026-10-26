{ config, lib, ... }:
let
  inherit (lib) mkIf;

  playwrightMcpEnabled =
    config.meow.home.enable
    && config.home-manager.users.${config.meow.home.user}.mHome.ai.playwright-mcp.enable;
in
{
  config = mkIf playwrightMcpEnabled {
    environment.etc."chromium/policies/managed/playwright-extension.json".text = builtins.toJSON {
      ExtensionSettings."mmlmfjhmonkocbjadbfplnigmagldckm" = {
        installation_mode = "force_installed";
        # Helium's pending-extension manager rewrites this placeholder to its
        # extension proxy; the ordinary Chrome Web Store URL bypasses that path.
        update_url = "https://update-url-to-be-replaced.qjz9zk";
      };
    };
  };
}
