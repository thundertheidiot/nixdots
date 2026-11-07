{
  pkgs,
  lib,
  config,
  inputs,
  theme,
  ...
}:
let
  inherit (lib) getExe mkIf mkMerge;
  inherit (builtins) toJSON;
  palette = theme.palette;
  gtk = config.mHome.themeGtk;
  qt = config.mHome.themeQt;
  noctalia = inputs.noctalia;
  # noctalia has a built in theme renderer apparently
  renderer = noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default;
  themeJson = pkgs.writeText "madoromi-noctalia-theme.json" (toJSON palette.noctalia);
  generated =
    pkgs.runCommand "madoromi-toolkit-${theme.variant}"
      {
        nativeBuildInputs = [ renderer ];
      }
      ''
        # keep the renderer away from live user settings
        export HOME="$TMPDIR/home"
        export XDG_CACHE_HOME="$TMPDIR/cache"
        export XDG_CONFIG_HOME="$TMPDIR/config"
        mkdir -p "$out" "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME"
        ${getExe renderer} theme --theme-json ${themeJson} --default-mode ${
          if palette.dark then "dark" else "light"
        } \
          -r ${noctalia}/assets/templates/gtk/gtk3.css:$out/gtk3.css \
          -r ${noctalia}/assets/templates/gtk/gtk4.css:$out/gtk4.css \
          -r ${noctalia}/assets/templates/qt/qtct.conf:$out/qtct.conf \
          -r ${noctalia}/assets/templates/kde/kcolorscheme.colors:$out/kcolorscheme.colors
        # the gtk templates import this even without extra tweaks
        touch "$out/noctalia-extensions.css"
      '';
in
mkIf (theme.family == "madoromi") (mkMerge [
  (mkIf gtk {
    gtk = {
      enable = true;
      # let our css handle gtk4 instead of a theme package
      gtk4.theme = null;
      theme = {
        package = pkgs.adw-gtk3;
        name = if palette.dark then "adw-gtk3-dark" else "adw-gtk3";
      };
      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus";
      };
    };

    xdg.configFile = {
      "gtk-3.0/gtk.css".source = "${generated}/gtk3.css";
      "gtk-3.0/noctalia-extensions.css".source = "${generated}/noctalia-extensions.css";
      "gtk-4.0/gtk.css".source = "${generated}/gtk4.css";
      "gtk-4.0/gtk-dark.css".source = "${generated}/gtk4.css";
      "gtk-4.0/noctalia-extensions.css".source = "${generated}/noctalia-extensions.css";
    };
  })
  (mkIf qt {
    qt = {
      enable = true;
      platformTheme.name = "qtct";
      # fusion gives the generated palette a plain base
      style.name = "Fusion";
    };
    xdg.configFile = {
      "qt5ct/qt5ct.conf".text = ''
        [Appearance]
        style=Fusion
        color_scheme_path=${generated}/qtct.conf
        custom_palette=true
      '';
      "qt6ct/qt6ct.conf".text = ''
        [Appearance]
        style=Fusion
        color_scheme_path=${generated}/qtct.conf
        custom_palette=true
      '';
    };
    xdg.dataFile."color-schemes/Madoromi.colors".source = "${generated}/kcolorscheme.colors";
  })
])
