{
  pkgs,
  lib,
  mlib,
  config,
  ...
}: let
  inherit (lib) mkIf mkForce;
  inherit (builtins) elem;
  inherit (mlib) homeModule;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "hyprland" env) (homeModule {
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
  });
}
