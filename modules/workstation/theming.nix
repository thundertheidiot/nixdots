{
  system = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.enable) {
      environment.systemPackages = with pkgs; [
        udev-gothic-nf
        cantarell-fonts
        noto-fonts-color-emoji
        (pkgs.catppuccin-gtk.override {
          accents = ["mauve"];
          size = "compact";
          variant = "mocha";
        })
        pkgs.papirus-icon-theme
        pkgs.catppuccin-cursors.mochaLavender
      ];

      # Catppuccin tty
      boot.kernelParams = [
        "vt.default_red=30,243,166,249,137,245,148,186,88,243,166,249,137,245,148,166"
        "vt.default_grn=30,139,227,226,180,194,226,194,91,139,227,226,180,194,226,173"
        "vt.default_blu=46,168,161,175,250,231,213,222,112,168,161,175,250,231,213,200"
      ];

      fonts.fontconfig = {
        enable = true;
        defaultFonts.serif = ["Cantarell"];
        defaultFonts.sansSerif = ["Cantarell"];
        defaultFonts.monospace = ["UDEV Gothic 35NF"];
        defaultFonts.emoji = ["Noto Color Emoji"];
      };

    };

  home = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.enable) (let
      cursor_package = pkgs.catppuccin-cursors.mochaLavender;
      cursor_name = "Catppuccin-Mocha-Lavender-Cursors";

      colors = with config.scheme.withHashtag; {
        background = base00;
        foreground = base07;
        inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
      };
    in {
      home.packages = with pkgs; [
        jetbrains-mono
        meslo-lgs-nf
        udev-gothic-nf
        cantarell-fonts
      ];

      fonts.fontconfig.enable = true;

      xresources = {
        path = "${config.xdg.configHome}/xresources";
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
        # gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
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

      xdg.configFile = {
        "gtk-4.0/assets".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
        "gtk-4.0/gtk.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
        "gtk-4.0/gtk-dark.css".source = "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
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
