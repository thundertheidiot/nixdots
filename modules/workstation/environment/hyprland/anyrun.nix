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
        programs.anyrun = {
          enable = true;

          config = {
            x.fraction = 0.5;
            y.fraction = 0.5;
            width.fraction = 0.5;
            height.fraction = 0.5;

            closeOnClick = true;

            plugins = map (l: "${pkgs.anyrun}/lib/lib${l}.so") [
              "applications"
              "rink"
              "websearch"
              "shell"
              "nix_run"
            ];
          };

          extraConfigFiles = {
            "keybinds.ron".text = ''
              Config(
                keybinds: [
                  Keybind(
                    key: "<Control>j",
                    action: "Down",
                  ),
                  Keybind(
                    key: "<Control>k",
                    action: "Up",
                  ),
                  Keybind(
                    key: "<Control>g",
                    action: "Close",
                  ),
                  Keybind(
                    key: "Escape",
                    action: "Close",
                  ),
                  Keybind(
                    key: "Return",
                    action: "Select",
                  ),
                ],
              )
            '';
          };
        };
      }
    ];
  };
}
