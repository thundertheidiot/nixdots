{
  inputs,
  mlib,
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt mkEnOptTrue;
  inherit (lib) mkIf mkMerge mapAttrs recursiveUpdate;

  cfg = config.mHome.browser;
in {
  options = {
    mHome.browser = {
      firefox = {
        enable = mkEnOpt "Firefox";
        defaults = mkEnOptTrue "Default tweaks";
      };

      zen = {
        enable = mkEnOpt "Zen";
        defaults = mkEnOptTrue "Default tweaks";
      };
    };
  };

  imports = [
    inputs.zen-browser.homeModules.default
  ];

  config = let
    defaultExtensions = import ./extensions.nix;
    engines = import ./search.nix;

    commonPolicies = {
      DisableAppUpdate = true;
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DontCheckDefaultBrowser = true;

      FirefoxSuggest = {
        WebSuggestions = false;
        SponsoredSuggestions = false;
        ImproveSuggest = false;
      };
    };

    policies = {
      ExtensionSettings =
        mapAttrs (_: v: {
          installation_mode = "force_installed";
          install_url = v;
          private_browsing = true;
        })
        defaultExtensions;
    };

    search = {
      force = true;
      default = "ddg";
      privateDefault = "ddg";
      inherit engines;
    };
  in (mkMerge [
    (mkIf cfg.zen.enable {
      programs.zen-browser = {
        enable = true;
        nativeMessagingHosts = [pkgs.firefoxpwa];

        policies = commonPolicies;

        profiles."nix-managed" = {
          id = 0;
        };
      };
    })
    (mkIf cfg.zen.defaults {
      programs.zen-browser = {
        inherit policies;

        profiles."nix-managed" = {
          inherit search;
        };
      };
    })
    (mkIf cfg.firefox.enable {
      programs.firefox = {
        enable = true;

        policies = recursiveUpdate commonPolicies {
          # firefox color for stylix
          ExtensionSettings."FirefoxColor@mozilla.com" = {
            installation_mode = "force_installed";
            install_url = "https://addons.mozilla.org/firefox/downloads/latest/firefox-color/addon-4757633-latest.xpi";
            private_browsing = true;
          };
        };

        profiles."nix-managed" = {
          id = 0;

          settings = {
            "privacy.trackingprotection.enabled" = true;
            "privacy.trackingprotection.socialtracking.enabled" = true;
            "browser.contentblocking.category" = "strict";
          };

          extensions.force = true;
        };
      };

      stylix.targets.firefox.profileNames = ["nix-managed"];
      stylix.targets.firefox.colorTheme.enable = true;
    })
    (mkIf cfg.firefox.defaults {
      programs.firefox = {
        inherit policies;

        profiles."nix-managed" = {
          inherit search;
        };
      };
    })
  ]);
}
