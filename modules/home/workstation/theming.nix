{
  config,
  pkgs,
  lib,
  ...
}: let
  cursor_package = pkgs.catppuccin-cursors.mochaLavender;
  cursor_name = "Catppuccin-Mocha-Lavender-Cursors";

  colors = with config.scheme.withHashtag; {
    background = base00;
    foreground = base07;
    inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
  };
in {
  config = lib.mkIf (config.setup.userMachine.enable) (with config; {
    home.packages = with pkgs; [
      jetbrains-mono
      meslo-lgs-nf
      udev-gothic-nf
      cantarell-fonts
    ];

    fonts.fontconfig.enable = true;

    xresources = {
      path = "${xdg.configHome}/xresources";
      properties = {
        "Nsxiv.window.background" = "${colors.background}";
        "Nsxiv.window.foreground" = "${colors.foreground}";
        "Nsxiv.mark.foreground" = "${colors.base04}";

        "Nsxiv.bar.background" = "${colors.foreground}";
        "Nsxiv.bar.foreground" = "${colors.background}";
      };
    };

    gtk = {
      enable = true;
      gtk2.configLocation = "${xdg.configHome}/gtk-2.0/gtkrc";
      font = {
        package = pkgs.cantarell-fonts;
        name = "Cantarell";
        size = 12;
      };
      theme = {
        package = pkgs.catppuccin-gtk.override {
          accents = ["mauve"];
          size = "compact";
          variant = "mocha";
        };
        name = "Catppuccin-Mocha-Compact-Mauve-Dark";
      };
      cursorTheme = {
        package = cursor_package;
        name = cursor_name;
        size = 24;
      };
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus-Dark";
      };
    };

    qt = {
      enable = true;
      platformTheme = "gnome";
      style = {
        name = "adwaita-dark";
        # package = pkgs.catppuccin-kde;
      };
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

    programs.fish.shellAliases = {
      "sxiv" = "nsxiv";
    };
  });
}
