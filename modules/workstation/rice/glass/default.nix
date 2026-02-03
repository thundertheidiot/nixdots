{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkForce;
  inherit (pkgs) replaceVars;

  cfg = config.meow.rice;
in {
  config = mkIf (cfg == "glass") {
    meow.home.modules = [
      {
        catppuccin.waybar.enable = false;
        programs.waybar.style = let
          colors = config.meow.workstation.theme.palette.withHashtag;
        in
          replaceVars ./waybar.css {
            accent = colors.base0D;

            fg = colors.base05;
            border = colors.base02;
            borderHover = colors.base03;
            muted = colors.base04;
            warn = colors.base0A;
            danger = colors.base08;

            inherit (colors) base00 base01 base02;
          };

        programs.alacritty.settings = {
          window = {
            opacity = mkForce 0.88;
            padding = {
              x = 12;
              y = 12;
            };
            dynamic_padding = true;
          };

          colors = {
            transparent_background_colors = false;
          };
        };

        wayland.windowManager.hyprland.settings = {
          general = {
            gaps_in = 5;
            gaps_out = 20;
            border_size = 1;
          };

          decoration = {
            rounding = 7;

            blur = {
              enabled = false;
              size = 3;
              passes = 1;
            };

            shadow = {
              enabled = true;
              range = 6;
              render_power = 4;
            };
          };

          animations.enabled = true;
        };

        programs.anyrun.extraCss = let
          colors = config.meow.workstation.theme.palette.withHashtag;
        in ''
          /* AnyRun CSS styled to match your Waybar look */

          /* --- Color tokens (same as your Waybar) --- */
          /* prettier-ignore-start */
          @define-color fg ${colors.base05};
          @define-color base00 ${colors.base00};
          @define-color base01 ${colors.base01};
          @define-color base02 ${colors.base02};
          @define-color border ${colors.base02};
          @define-color borderHover ${colors.base03};
          @define-color accent ${colors.base0D};
          @define-color muted ${colors.base04};
          @define-color danger ${colors.base08};
          @define-color warn ${colors.base0A};
          /* prettier-ignore-end */

          /* --- Convenience mappings for this sheet --- */
          @define-color theme_bg alpha(@base00, 0.88);
          @define-color pill_bg alpha(@base01, 0.7);
          @define-color pill_bg_hover alpha(@base02, 0.8);

          /* Keep compatibility with defaults that reference theme_* variables */
          @define-color theme_bg_color @theme_bg;
          @define-color theme_selected_bg_color @accent;

          /* --- Base typography --- */
          * {
          	border: none;
          	font-family:
          		Inter, System-ui, "Symbols Nerd Font", sans-serif;
          	font-size: 13.5px;
          	font-weight: 500;
          	min-height: 0;
          	color: @fg;
          }

          /* Float the window; compositor can blur the background if enabled */
          window {
          	background: transparent;
          }

          /* Glassy container like your Waybar root box */
          box.main {
          	padding: 6px;
          	margin: 10px 14px 0 14px;
          	border-radius: 14px;
          	border: 1px solid @border;
          	background-color: @theme_bg;
          	box-shadow:
          		0 18px 40px rgba(0, 0, 0, 0.35),
          		0 2px 6px rgba(0, 0, 0, 0.22);
          }

          /* Search entry styled like a “pill” module */
          text {
          	min-height: 30px;
          	padding: 6px 10px;
          	border-radius: 12px;
          	background: @pill_bg;
          	border: 1px solid @border;
          	transition:
          		background 160ms ease,
          		border-color 160ms ease,
          		color 160ms ease,
          		box-shadow 160ms ease;
          }

          text:focus {
          	background: @pill_bg_hover;
          	border-color: @borderHover;
          	box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.04);
          }

          /* Matches container: transparent so pills stand out */
          .matches {
          	background-color: transparent;
          	border-radius: 12px;
          	margin-top: 6px;
          }

          /* Each plugin section spacing */
          box.plugin:first-child {
          	margin-top: 6px;
          }

          box.plugin.info {
          	min-width: 200px;
          	background: @pill_bg;
          	border: 1px solid @border;
          	border-radius: 12px;
          	padding: 4px 10px;
          }

          /* Plugin lists keep the glass background visible */
          list.plugin {
          	background-color: transparent;
          }

          /* Text sizes and emphasis */
          label.match.description {
          	font-size: 10px;
          	color: @muted;
          }

          label.plugin.info {
          	font-size: 14px;
          	color: @fg;
          }

          /* Individual match rows as modern pills */
          .match {
          	background: @pill_bg;
          	border: 1px solid @border;
          	border-radius: 12px;
          	padding: 6px 10px;
          	margin: 4px 0;
          	transition:
          		background 160ms ease,
          		border-color 160ms ease,
          		color 160ms ease,
          		box-shadow 160ms ease;
          }

          .match:hover {
          	background: @pill_bg_hover;
          	border-color: @borderHover;
          	box-shadow: inset 0 0 0 1px rgba(255, 255, 255, 0.04);
          }

          /* Selected: subtle accent cue, keep it glassy */
          .match:selected {
          	background: @pill_bg_hover;
          	border-color: @borderHover;
          	border-left: 4px solid @accent;
          	animation: fade 0.1s linear;
          }

          /* Optional: emphasize primary label on selection */
          .match:selected label {
          	color: @fg;
          }

          /* State-like colors if any plugin uses them */
          .match.warning label {
          	color: @warn;
          }
          .match.critical label {
          	color: @danger;
          }

          /* Entry + list edges align nicely inside the container */
          text,
          .matches {
          	margin-left: 2px;
          	margin-right: 2px;
          }

          /* Fade-in animation like your Waybar tooltip blink cadence */
          @keyframes fade {
          	0% {
          		opacity: 0;
          	}
          	100% {
          		opacity: 1;
          	}
          }
        '';
      }
    ];
  };
}
