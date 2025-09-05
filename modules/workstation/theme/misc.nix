{
  mlib,
  config,
  ...
}: let
  inherit (mlib) homeModule;
in {
  config = homeModule {
    programs.tofi = {
      settings = {
        border-width = 1;
        outline-width = 0;
        corner-radius = 7;
        prompt-text = "run: ";
      };
    };

    services.mako = {
      settings = {
        margin = "10";
        padding = "5";
        border-size = 2;
        border-radius = 6;
        icons = true;
        max-icon-size = 32;
        default-timeout = 2000;
        ignore-timeout = true;
      };
    };

    # programs.waybar.style = let
    #   colors = config.lib.stylix.colors.withHashtag;
    # in ''
    #   * {
    #       all: unset;
    #       border: none;
    #       border-radius: 4px;
    #       font-family: Cantarell;
    #       font-size: 12px;
    #       min-height: 0;
    #   }

    #   window#waybar {
    #       background: @theme_base_color;
    #       background-color: ${colors.base00};
    #       border-bottom: 3px solid ${colors.base08};
    #       color: @theme_text_color;
    #       transition-property: background-color;
    #       transition-duration: .5s;
    #       border-radius: 0;
    #   }

    #   window#waybar.hidden {
    #       opacity: 0.2;
    #   }

    #   tooltip {
    #     background: ${colors.base00};
    #     border: 1px solid ${colors.base05};
    #   }

    #   tooltip label {
    #     color: @theme_text_color;
    #   }

    #   /*
    #   window#waybar.empty {
    #       background-color: transparent;
    #   }
    #   window#waybar.solo {
    #       background-color: #FFFFFF;
    #   }
    #   */

    #   #workspaces button {
    #       padding: 0 0.7em;
    #       background-color: transparent;
    #       color: ${colors.base05};
    #       box-shadow: inset 0 -3px transparent;
    #   }

    #   #workspaces button:hover {
    #       background: rgba(0, 0, 0, 0.2);
    #       box-shadow: inset 0 -3px ${colors.base05};
    #   }

    #   #workspaces button.urgent {
    #       background-color: ${colors.base01};
    #   }

    #   #clock,
    #   #battery,
    #   #cpu,
    #   #memory,
    #   #disk,
    #   #temperature,
    #   #backlight,
    #   #network,
    #   #pulseaudio,
    #   #custom-weather,
    #   #tray,
    #   #mode,
    #   #idle_inhibitor,
    #   #custom-notification,
    #   #sway-scratchpad,
    #   #mpd {
    #       padding: 0 10px;
    #       margin: 6px 3px;
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }

    #   #window,
    #   #workspaces {
    #       margin: 0 4px;
    #   }

    #   /* If workspaces is the leftmost module, omit left margin */
    #   .modules-left > widget:first-child > #workspaces {
    #       margin-left: 0;
    #   }

    #   /* If workspaces is the rightmost module, omit right margin */
    #   .modules-right > widget:last-child > #workspaces {
    #       margin-right: 0;
    #   }

    #   #clock {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }

    #   #battery {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }

    #   #battery.charging, #battery.plugged {
    #       color: ${colors.base00};
    #       background-color: ${colors.base15};
    #   }

    #   @keyframes blink {
    #       to {
    #           background-color: #ffffff;
    #           color: #000000;
    #       }
    #   }

    #   #battery.critical:not(.charging) {
    #       background-color: ${colors.base08};
    #       color: ${colors.base00};
    #       animation-name: blink;
    #       animation-duration: 0.5s;
    #       animation-timing-function: linear;
    #       animation-iteration-count: infinite;
    #       animation-direction: alternate;
    #   }

    #   label:focus {
    #       background-color: #000000;
    #   }

    #   #cpu {
    #       background-color: ${colors.base03};
    #       color: ${colors.base00};
    #   }

    #   #memory {
    #       background-color: ${colors.base03};
    #       color: ${colors.base00};
    #   }

    #   #backlight {
    #       background-color: ${colors.base03};
    #   }

    #   #network {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }

    #   #network.disconnected {
    #       background-color: ${colors.base08};
    #       color: ${colors.base00};
    #   }

    #   #pulseaudio {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }

    #   #pulseaudio.muted {
    #       background-color: ${colors.base08};
    #       color: ${colors.base00};
    #   }

    #   #temperature {
    #       background-color: ${colors.base12};
    #       color: ${colors.base00};
    #   }

    #   #temperature.critical {
    #       background-color: ${colors.base04};
    #       color: ${colors.base00};
    #   }

    #   #tray {
    #       background-color: ${colors.base05}
    #   }

    #   #tray > .passive {
    #       -gtk-icon-effect: dim;
    #   }

    #   #tray > .needs-attention {
    #       -gtk-icon-effect: highlight;
    #       background-color: ${colors.base04};
    #   }

    #   #idle_inhibitor {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }

    #   #idle_inhibitor.activated {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }

    #   #mpd {
    #       background-color: ${colors.base02};
    #       color: ${colors.base00};
    #   }

    #   #mpd.disconnected {
    #       background-color: ${colors.base04};
    #   }

    #   #mpd.stopped {
    #       background-color: ${colors.base00};
    #   }

    #   #mpd.paused {
    #       background-color: ${colors.base08};
    #   }

    #   #language {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #       padding: 0 5px;
    #       margin: 6px 3px;
    #       min-width: 16px;
    #   }

    #   #keyboard-state {
    #       background-color: ${colors.base00};
    #       color: ${colors.base00};
    #       padding: 0 0px;
    #       margin: 0 5px;
    #       min-width: 16px;
    #   }

    #   #keyboard-state > label {
    #       padding: 0 5px;
    #   }

    #   #keyboard-state > label.locked {
    #       background: rgba(0, 0, 0, 0.2);
    #   }

    #   #disk {
    #       background-color: ${colors.base05};
    #       color: ${colors.base00};
    #   }
    # '';
  };
}
