{
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;

  cfg = config.meow.workstation.theming;
  theme = config.meow.workstation.theme;
in {
  config = mkIf (cfg.enable && theme == "catppuccin-mocha") {
    boot.kernelParams = [
      # mocha tty
      "vt.default_red=30,243,166,249,137,245,148,186,88,243,166,249,137,245,148,166"
      "vt.default_grn=30,139,227,226,180,194,226,194,91,139,227,226,180,194,226,173"
      "vt.default_blu=46,168,161,175,250,231,213,222,112,168,161,175,250,231,213,200"
    ];

    meow.home.modules = [
      ({config, ...}: let
        gtkPackage = pkgs.catppuccin-gtk.override {
          accents = ["mauve"];
          size = "compact";
          variant = "mocha";
        };
        gtkName = "catppuccin-mocha-mauve-compact";
      in {
        gtk = {
          theme = lib.mkForce {
            package = gtkPackage;
            name = gtkName;
          };
          iconTheme = lib.mkForce {
            inherit (cfg.iconTheme) package name;
          };
        };

        qt = {
          enable = true;
          # platformTheme.name = "kde6";
          style = {
            # name = "kvantum";
          };
        };

        xdg.dataFile."Kvantum/catppuccin-mocha-mauve" = {
          source = "${(pkgs.catppuccin-kvantum.override {
            accent = "mauve";
            variant = "mocha";
          })}/share/Kvantum/catppuccin-mocha-mauve";
          recursive = true;
        };

        xdg.configFile = let
          s = source: lib.mkForce {inherit source;};
        in {
          "Kvantum/kvantum.kvconfig".text = ''
            [General]
            theme=catppuccin-mocha-mauve
          '';
          "gtk-4.0/assets" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
          "gtk-4.0/gtk.css" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
          "gtk-4.0/gtk-dark.css" = s "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
        };
      })
      (mkIf (builtins.elem "plasma" config.meow.workstation.environment) (let
        qtPackage = pkgs.catppuccin-kde.override {
          accents = ["mauve"];
          flavour = ["mocha"];
        };
      in {
        home.packages = with pkgs; [
          qtPackage
          cfg.iconTheme.package
        ];

        programs.plasma = let
          V = val: {
            value = val;
            immutable = true;
          };
        in {
          configFile = {
            "auroraerc"."CatppuccinMocha-Modern"."ButtonSize" = V 0;
            "kdeglobals"."KDE"."LookAndFeelPackage" = V "Catppuccin-Mocha-Mauve";
            "kdeglobals"."Icons"."Theme" = V cfg.iconTheme.name;
            "kdedefaults/kdeglobals"."Icons"."Theme" = V cfg.iconTheme.name;
            "kdedefaults/kdeglobals"."General"."ColorScheme" = V "CatppuccinMochaMauve";
          };
        };
      }))
    ];
  };
}
