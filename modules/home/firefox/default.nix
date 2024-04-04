{
  config,
  pkgs,
  lib,
  ...
}: {
  config = lib.mkIf (config.setup.firefox.enable) (with config; {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox;

      policies = {
        DisableFirefoxStudies = true;
        DisablePocket = true;
        DisableTelemetry = true;
        DisableAppUpdate = true;
        FirefoxSuggest = {
          WebSuggestions = false;
          SponsoredSuggestions  = false;
          ImproveSuggest = false;
        };
        ExtensionSettings = {
          "uBlock0@raymondhill.net" = {
            installation_mode = "force_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/addon-11423598-latest.xpi";
          };
          "idcac-pub@guus.ninja" = {
            installation_mode = "force_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/addon-17568914-latest.xpi";
          };
        };
      };

      profiles."nix-managed" = {
        extensions = with pkgs.firefox-addons; [
          bitwarden
          privacy-redirect
        ];

        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
          engines = {
            "DuckDuckGo" = {
              urls = [{
                template = "https://duckduckgo.com/";
                params = [
                  { name = "q"; value = "{searchTerms}"; }
                ];
              }];
            };
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
            };
          };
        };
      };
    };
  });
}
