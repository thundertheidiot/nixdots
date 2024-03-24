{
  config,
  pkgs,
  ...
}: let
  cursor_package = pkgs.catppuccin-cursors.mochaLavender;
  cursor_name = "Catppuccin-Mocha-Lavender-Cursors";
in
  with config; {
    home.packages = with pkgs; [
      jetbrains-mono
      meslo-lgs-nf
    ];

    fonts.fontconfig.enable = true;

    home.file.".config/fontconfig/fonts.conf".text = ''
      <?xml version='1.0'?>
      <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
      <fontconfig>
      <alias>
      <family>sans-serif</family>
      <prefer>
      <family>Noto Sans</family>
      </prefer>
      </alias>

      <alias>
      <family>serif</family>
      <prefer>
      <family>Noto Serif</family>
      </prefer>
      </alias>

      <alias>
      <family>monospace</family>
      <prefer>
      <family>JetBrainsMono Nerd Font</family>
      <family>JetBrainsMono NFM</family>
      </prefer>
      </alias>
      </fontconfig>
    '';

    gtk = {
      enable = true;
      gtk2.configLocation = "${xdg.configHome}/gtk-2.0/gtkrc";
      font = {
        package = pkgs.cantarell-fonts;
        name = "Cantarell";
        size = 12;
      };
      theme = {
        package = (pkgs.catppuccin-gtk.override {
          accents = [ "mauve" ];
          size = "compact";
          variant = "mocha";
        });
        name = "Catppuccin-Mocha-Compact-Mauve-Dark";
      };
      cursorTheme = {
        package = cursor_package;
        name = cursor_name;
        size = 24;
      };
    };

    qt = {
      enable = true;
      platformTheme = "gtk3";
    };

    xdg.configFile = {
      "gtk-4.0/assets".source = "${gtk.theme.package}/share/themes/${gtk.theme.name}/gtk-4.0/assets";
      "gtk-4.0/gtk.css".source = "${gtk.theme.package}/share/themes/${gtk.theme.name}/gtk-4.0/gtk.css";
      "gtk-4.0/gtk-dark.css".source = "${gtk.theme.package}/share/themes/${gtk.theme.name}/gtk-4.0/gtk-dark.css";
    };

    home.pointerCursor = {
      package = cursor_package;
      name = cursor_name;
      size = 24;

      x11.defaultCursor = "left_ptr";
      x11.enable = true;
      gtk.enable = true;
    };
  }
