{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (lib) mkIf;
  inherit (builtins) elem;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "hyprland" env) {
    meow.home.modules = [
      {
        stylix.targets.swaync.enable = false;

        services.swaync = {
          enable = true;
          settings = {
            positionX = "right";
            positionY = "top";
            layer = "overlay";
            "layer-shell" = true;
            "layer-shell-cover-screen" = true;

            timeout = 5;
            "timeout-low" = 3;
            "timeout-critical" = 0;

            "notification-window-width" = 380;
            "notification-window-height" = -1;
            "control-center-width" = 380;
            "control-center-height" = 520;
            "transition-time" = 200;
            "relative-timestamps" = true;
            "notification-grouping" = true;
            "hide-on-action" = true;
            "hide-on-clear" = false;

            # Widgets layout
            widgets = [
              "inhibitors"
              "title"
              "dnd"
              "mpris"
              "volume"
              "notifications"
            ];

            "widget-config" = {
              title = {
                text = "Notifications";
                "clear-all-button" = true;
                "button-text" = "Clear All";
              };
              notifications = {
                vexpand = true;
              };
              mpris = {
                "show-album-art" = "when-available";
                autohide = true;
                "loop-carousel" = false;
              };
              volume = {
                "show-per-app" = false;
                "show-per-app-icon" = true;
                "show-per-app-label" = false;
              };
            };
          };

          style = let
            colors = config.lib.stylix.colors.withHashtag;
          in
            pkgs.replaceVars ./swaync.css {
              accent = colors.base0D;

              fg = colors.base05;
              border = colors.base02;
              borderHover = colors.base03;
              muted = colors.base04;
              warn = colors.base0A;
              danger = colors.base08;

              inherit (colors) base00 base01 base02;
            };
        };
      }
    ];
  };
}
