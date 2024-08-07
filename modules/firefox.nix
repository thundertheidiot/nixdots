{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) homeModule mkOpt mkEnOpt;
  inherit (lib) mkIf;
  inherit (builtins) mapAttrs;

  inherit (lib.types) attrsOf str;

  cfg = config.meow.firefox;
in {
  options = {
    meow.firefox = {
      enable = mkEnOpt "Install and configure firefox.";
      addons =
        mkOpt (attrsOf str) {
          "uBlock0@raymonhill.net" = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/addon-11423598-latest.xpi";
          "idcac-pub@guus.ninja" = "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/addon-17568914-latest.xpi";
          "{446900e4-71c2-419f-a6a7-df9c091e268b}" = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/addon-12533945-latest.xpi";
        } {
          description = ''
            Firefox addons to install.
            To find the addon ID, go to <about:debugging#/runtime/this-firefox>
            For the addon URL, go to the addons.mozilla.org page, and substitute the missing values in this template:
            `https://addons.mozilla.org/firefox/downloads/latest/ADDON_NAME_ID/addon-ADDON_ACCOUNT_ID-latest.xpi`
            ADDON_NAME_ID should be in the url bar, ADDON_ACCOUNT_ID is in the link to the addon creator's page
          '';
        };
    };
  };

  config = mkIf cfg.enable (homeModule {
    programs.firefox = {
      enable = true;

      policies = {
        DisableFirefoxStudies = true;
        DisablePocket = true;
        DisableTelemetry = true;
        DisableAppUpdate = true;
        FirefoxSuggest = {
          WebSuggestions = false;
          SponsoredSuggestions = false;
          ImproveSuggest = false;
        };
        ExtensionSettings =
          mapAttrs (_: v: {
            installation_mode = "force_installed";
            install_url = v;
          })
          cfg.addons;

        # ExtensionSettings = {
        #   # uBlockOrigin
        #   "uBlock0@raymondhill.net" = {
        #     installation_mode = "force_installed";
        #     install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/addon-11423598-latest.xpi";
        #   };
        #   # I still don't care about cookies
        #   "idcac-pub@guus.ninja" = {
        #     installation_mode = "force_installed";
        #     install_url = "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/addon-17568914-latest.xpi";
        #   };
      };

      profiles."nix-managed" = {
        settings = {
          "dom.private-attribution.submission.enabled" = false;
          "browser.contentblocking.category" = "strict";
        };

        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
          engines = {
            "DuckDuckGo" = {
              urls = [
                {
                  template = "https://duckduckgo.com/";
                  params = [
                    {
                      name = "q";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
            "Nix Packages" = {
              urls = [
                {
                  template = "https://search.nixos.org/packages";
                  params = [
                    {
                      name = "query";
                      value = "{searchTerms}";
                    }
                  ];
                }
              ];
            };
          };
        };
      };
    };
  });
}
