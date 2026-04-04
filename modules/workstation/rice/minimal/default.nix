{
  lib,
  config,
  ...
}: let
  inherit (lib) mkIf mkForce;
  cfg = config.meow.rice;
in {
  imports = [
    ./waybar.nix
  ];

  config = mkIf (cfg == "minimal") {
    meow.home.modules = [
      {
        programs.alacritty.settings = {
          window = {
            opacity = mkForce 1.0;
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
      }
    ];
  };
}
