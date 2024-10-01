# TODO: move under workstation
{
  system = {...}: {};

  home = {
    mlib,
    lib,
    config,
    pkgs,
    ...
  }: {
    imports = [
      ./waybar.nix
    ];

    config = lib.mkIf (builtins.elem "hyprland" config.workstation.environment) {
      home.packages = with pkgs; [
        tofi
        wl-clipboard
        wlr-randr
        swayosd
      ];

      home.file.".config/swappy/config".text = ''
        [Default]
        save_dir=${config.xdg.userDirs.pictures}/screenshots
        save_filename_format=annotated-%Y-%m-%d_%H-%M-%S.png
      '';

      stylix.targets.tofi.enable = true;
      programs.tofi = {
        enable = true;
        settings = {
          border-width = 2;
          outline-width = 0;
          corner-radius = 6;
          prompt-text = "run: ";
        };
      };

      services.mako = {
        enable = true;
        margin = "10";
        padding = "5";
        borderSize = 2;
        borderRadius = 6;
        icons = true;
        maxIconSize = 32;
        defaultTimeout = 2000;
        ignoreTimeout = true;
      };
    };
  };
}
