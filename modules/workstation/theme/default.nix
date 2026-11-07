{
  config,
  pkgs,
  mlib,
  lib,
  ...
}:
let
  inherit (mlib) mkOpt;
  inherit (builtins) map substring;
  inherit (lib) mkIf;
  inherit (lib.types) attrs enum str;

  theme = config.meow.workstation.theme;
  catEnabled = theme.family == "catppuccin";
in
{
  options.meow.workstation.theme = {
    family = mkOpt (enum [
      "catppuccin"
      "madoromi"
    ]) "madoromi" { description = "desktop palette family"; };
    variant = mkOpt str (if theme.family == "madoromi" then "fuyu-yoru" else "mocha") {
      example = "haru-hiru";
      description = "palette variant within the selected family";
    };
    palette =
      let
        palettes = {
          madoromi = import ./palettes/madoromi.nix { inherit (theme) variant; };
          catppuccin = import ./palettes/catppuccin.nix {
            source = config.catppuccin.sources.palette;
            inherit (theme) variant;
          };
        };
      in
      mkOpt attrs (mlib.mkThemePalette palettes.${theme.family}) { }
      // {
        readOnly = true;
      };
  };

  config = mkIf config.meow.workstation.enable {
    console.colors = mkIf (!catEnabled) (map (color: substring 1 6 color) theme.palette.ansi);

    catppuccin = {
      enable = catEnabled;
      autoEnable = catEnabled;
      flavor = mkIf catEnabled theme.variant;
      accent = mkIf catEnabled "mauve";
      plymouth.enable = false;
    };

    # fonts
    fonts = {
      packages = with pkgs; [
        inter-nerdfont
        nerd-fonts.symbols-only
        # cantarell-fonts
        maple-mono.NF-CN
        noto-fonts-color-emoji
      ];

      fontconfig = {
        enable = true;
        useEmbeddedBitmaps = true;
        defaultFonts = {
          serif = [ "Inter Nerd Font" ];
          sansSerif = [ "Inter Nerd Font" ];
          monospace = [
            "Maple Mono NF CN"
            "Symbols Nerd Font"
          ];
          emoji = [ "Noto Color Emoji" ];
        };
      };
    };

    # cursor
    environment.variables.XCURSOR_SIZE = 24;

    # plymouth
    boot = {
      plymouth = {
        enable = true;

        themePackages = with pkgs; [
          (plymouth-blahaj-theme.overrideAttrs (prev: {
            postPatch = ''
              substituteInPlace "blahaj.plymouth" \
                --replace-fail "=0x000000" "=0x${
                  substring 1 6 (if catEnabled then theme.palette.roles.surface else theme.palette.roles.outer)
                }"
            '';
          }))
        ];

        theme = "blahaj";
      };

      loader.timeout = 5;

      consoleLogLevel = 0;

      kernelParams = [
        "quiet"
        "splash"
        "boot.shell_on_fail"
        "loglevel=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
        "udev.log_priority=3"
      ];
    };
  };
}
