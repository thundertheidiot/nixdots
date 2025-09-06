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
  imports = [
    ./firefox.nix
  ];

  config = mkIf (cfg == "glass") {
    meow.home.modules = [
      {
        programs.waybar.style = let
          colors = config.lib.stylix.colors.withHashtag;
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

        services.swaync.style = let
          colors = config.lib.stylix.colors.withHashtag;
        in
          replaceVars ./swaync.css {
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
              enabled = true;
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
      }
    ];
  };
}
