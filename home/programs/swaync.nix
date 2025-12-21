{...}: {
  config = {
    stylix.targets.swaync.enable = false;

    services.swaync = {
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
            "loop-carousel" = true;
          };
          volume = {
            "show-per-app" = false;
            "show-per-app-icon" = true;
            "show-per-app-label" = false;
          };
        };
      };
    };
  };
}
