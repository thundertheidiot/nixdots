{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (lib) mkIf;
  inherit (lib.types) bool str attrs;
  inherit (lib.attrsets) mergeAttrsList;
  inherit (mlib) mkOpt mkEnOpt defineJqDeepmerge applyWithJq jqMergeFileWithValue;
  inherit (builtins) toJSON;
  cfg = config.meow.program.discordConfig;
in {
  options = {
    meow.program.discordConfig = {
      enable = mkOpt bool true {
        description = "Discord";
      };

      override = {
        quickCss = mkEnOpt "Override quickcss completely";
        settings = mkEnOpt "Override settings completely";
        userSettings = mkEnOpt "Override userSettings completely";
        plugins = mkEnOpt "Override plugins completely";
      };

      settings = mkOpt attrs {} {
        description = "Vesktop settings.json file";
      };
      userSettings = mkOpt attrs {} {
        description = "Vesktop settings/settings.json file";
      };
      plugins = mkOpt attrs {} {
        description = "Vesktop plugin configuration";
      };

      quickCss = mkOpt str "" {
        description = "Additional quickcss";
      };
    };
  };

  config = mkIf (config.meow.program.discord && cfg.enable) (let
    defaultSettings = {
      discordBranch = "stable";
      splashColor = "rgb(205, 214, 244)";
      splashBackground = "rgb(30, 30, 46)";
      staticTitle = true;
      minimizeToTray = false;
      tray = false;
    };

    defaultUserSettings = {
      autoUpdate = true;
      autoUpdateNotification = true;
      useQuickCss = true;
      disableMinSize = true;
      # themeLinks = [
      #   "https://catppuccin.github.io/discord/dist/catppuccin-mocha.theme.css"
      # ];
      enabledThemes = [
        "stylix.theme.css"
      ];
    };

    defaultQuickCss = ''
      /* Hide Nitro gift button */
      button[aria-label="Send a gift"] {
          display: none;
      }
      /* Hide sticker picker button */
      button[aria-label="Open sticker picker"] {
          display: none;
      }
      /* Hide annoying sticker popup window that appears when you type */
      .channelTextArea-1FufC0 > .container-1ZA19X {
          display: none;
      }
    '';

    defaultPlugins = import ./plugins.nix;

    finalSettings =
      if cfg.override.settings
      then cfg.settings
      else mergeAttrsList [defaultSettings cfg.settings];
    finalPlugins =
      if cfg.override.plugins
      then cfg.plugins
      else mergeAttrsList [defaultPlugins cfg.plugins];
    finalUserSettings =
      if cfg.override.userSettings
      then mergeAttrsList [cfg.userSettings {plugins = finalPlugins;}]
      else mergeAttrsList [defaultUserSettings cfg.userSettings {plugins = finalPlugins;}];
  in {
    meow.home.configFile."vesktop/quickCss.css" = {
      enable = true;
      text =
        if cfg.override.quickCss
        then cfg.quickCss
        else defaultQuickCss + cfg.quickCss;
    };

    nixpkgs.overlays = let
      path = f: "${config.meow.home.directory}/.config/vesktop/${f}";
      jq = "${pkgs.jq}/bin/jq";

      makeSettings = pkgs.writeShellScript "create_discord_settings" ''
        ${jqMergeFileWithValue {
          inherit jq;
          file = path "settings.json";
          value = finalSettings;
        }}
        ${jqMergeFileWithValue {
          inherit jq;
          file = path "settings/settings.json";
          value = finalUserSettings;
        }}
      '';
    in [
      (final: prev: {
        vesktop = prev.vesktop.overrideAttrs (old: {
          postFixup =
            builtins.replaceStrings
            ["$out/bin/vesktop"] # makeWrapper command
            
            ["$out/bin/vesktop --run ${makeSettings}"]
            old.postFixup;
        });
      })
    ];
  });
}
