{ lib, pkgs, config, ... }: let
  colors = with config.scheme.withHashtag; {
    background = base00;
    foreground = base07;
    inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
  };
in {
  imports = [
    ./swayfx
    ./hyprland
    ./waybar.nix
  ];

  config = lib.mkIf (config.setup.hyprland.enable || config.setup.swayfx.enable) {
    home.file.".config/swappy/config".text = ''
      [Default]
      save_dir=${config.xdg.userDirs.pictures}/screenshots
      save_filename_format=annotated-%Y-%m-%d_%H-%M-%S.png
    '';

    xdg.configFile."tofi/config" = {
      enable = true;
      text = ''
        font = "Monospace"
        font-size = 12
        border-width = 2
        outline-width = 0
        corner-radius = 6

        text-color = ${colors.foreground}
        text-background = ${colors.background}

        background-color = ${colors.background}
        border-color = ${colors.base04}

        selection-color = ${colors.base04}
        selection-background = ${colors.background}

        text-cursor-style = bar
        text-cursor-color = ${colors.foreground}
        text-cursor-background = ${colors.background}

        prompt-text = "run: "
      '';
    };

    programs.bemenu = {
      enable = true;
      settings = {
        line-height = 30;
        ignorecase = true;
        tb = colors.background;
        tf = colors.foreground;
        fb = colors.background;
        ff = colors.foreground;
        cb = colors.foreground;
        cf = colors.foreground;
        nb = colors.background;
        nf = colors.foreground;
        hb = colors.background;
        hf = colors.base04;
        sb = colors.background;
        sf = colors.base04;
      };
    };

    services.mako = {
      enable = true;
      backgroundColor = "${colors.background}FF";
      borderColor = "${colors.foreground}FF";
      font = "monospace 10";
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
}
