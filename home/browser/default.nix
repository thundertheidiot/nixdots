{
  inputs,
  mlib,
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt mkEnOptTrue;
  inherit (lib) mkIf mkMerge mapAttrs;

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
    };

    policies = {
      ExtensionSettings =
        mapAttrs (_: v: {
          installation_mode = "force_installed";
          install_url = v;
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

        policies = commonPolicies;

        profiles."nix-managed" = {
          id = 0;
        };
      };
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
