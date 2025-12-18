{
  config,
  lib,
  pkgs,
  mlib,
  inputs,
  ...
}: let
  inherit (lib) mkIf;
  inherit (mlib) homeModule;
  inherit (builtins) elem;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "niri" env) {
    environment.systemPackages = [pkgs.xwayland-satellite];
    programs.niri.enable = true;

    home-manager.sharedModules = [
      {
        programs.niri = {
          settings = {
            environment = {
              NIXOS_OZONE_WL = "1";
            };

            screenshot-path = "~/Pictures/screenshots/%Y-%m-%d_%H-%M-%S.png";
            prefer-no-csd = true;

            cursor.size = 24;

            input = {
              keyboard.repeat-delay = 300;
              keyboard.repeat-rate = 50;

              mouse.accel-profile = "flat";

              focus-follows-mouse.enable = true;
            };

            gestures.hot-corners.enable = true;

            binds = {
              "Mod+W".action.spawn = "firefox";
              "Mod+Return".action.spawn = "alacritty";
              "Mod+E".action.spawn-sh = "emacsclient -c -a ''";
              "Mod+Semicolon".action.spawn-sh = "emacsclient -c -a '' -e '(meow/eshell)'";

              "Print".action.screenshot = {};
            };
          };
        };
      }
    ];
  };
}
