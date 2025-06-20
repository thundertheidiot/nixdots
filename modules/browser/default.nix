{
  inputs,
  mlib,
  lib,
  config,
  ...
}: let
  inherit (mlib) homeModule mkEnOpt mkEnOptTrue;
  inherit (lib) mkIf mkMerge mapAttrs;

  cfg = config.meow.browser;
in {
  options = {
    meow.browser = {
      # firefox = {
      #   enable = mkEnOpt "Firefox";
      #   defaults = mkEnOptTrue "Default tweaks";
      # };

      # firedragon = {
      #   enable = mkEnOpt "Firedragon";
      #   defaults = mkEnOptTrue "Default tweaks";
      # };

      zen = {
        enable = mkEnOpt "Zen";
        defaults = mkEnOptTrue "Default tweaks";
      };
    };
  };

  config = homeModule ({
    config,
    pkgs,
    ...
  }: let
    defaultExtensions = {
      # ublock origin
      "uBlock0@raymondhill.net" = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/addon-11423598-latest.xpi";
      # i still don't care about cookies
      "idcac-pub@guus.ninja" = "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/addon-17568914-latest.xpi";
      # bitwarden
      "{446900e4-71c2-419f-a6a7-df9c091e268b}" = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/addon-12533945-latest.xpi";
    };

    engines = {
      "ddg" = {
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
        definedAliases = ["@ddg"];
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
        definedAliases = ["@np"];
      };
      "Nix Options" = {
        urls = [
          {
            template = "https://search.nixos.org/options";
            params = [
              {
                name = "query";
                value = "{searchTerms}";
              }
            ];
          }
        ];

        definedAliases = ["@no"];
      };
    };
  in {
    imports = [
      inputs.zen-browser.homeModules.default
    ];

    config = mkMerge [
      (mkIf cfg.zen.enable {
        programs.zen-browser = {
          enable = true;
          nativeMessagingHosts = [pkgs.firefoxpwa];

          policies = mkIf cfg.zen.defaults {
            ExtensionSettings =
              mapAttrs (_: v: {
                installation_mode = "force_installed";
                install_url = v;
              })
              defaultExtensions;
          };

          profiles."nix-managed" = {
            search = mkIf cfg.zen.defaults {
              force = true;
              default = "ddg";
              privateDefault = "ddg";
              inherit engines;
            };
          };
        };
      })
    ];
  });
}
