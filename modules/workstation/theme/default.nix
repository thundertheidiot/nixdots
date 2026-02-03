{
  config,
  pkgs,
  mlib,
  lib,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) attrs;
in {
  options.meow.workstation.theme.palette = mkOpt attrs (import ./mocha.nix) {};

  config = {
    catppuccin = {
      enable = true;
      flavor = "mocha";
      accent = "mauve";
    };

    # fonts
    fonts = {
      packages = with pkgs; [
        nerd-fonts.symbols-only
        pkgs.cantarell-fonts
        pkgs.maple-mono.NF-CN
        pkgs.noto-fonts-color-emoji
      ];

      fontconfig = {
        enable = true;
        useEmbeddedBitmaps = true;
        defaultFonts = {
          serif = ["Cantarell" "Noto Color Emoji"];
          sansSerif = ["Cantarell" "Noto Color Emoji"];
          monospace = ["Maple Mono NF CN" "Noto Color Emoji"];
        };
      };
    };

    # cursor
    environment.variables.XCURSOR_SIZE = 24;

    # plymouth
    catppuccin.plymouth.enable = false;
    boot = {
      plymouth = {
        enable = true;

        themePackages = with pkgs; [
          (plymouth-blahaj-theme.overrideAttrs (prev: {
            postPatch = ''
              substituteInPlace "blahaj.plymouth" \
                --replace "=0x000000" "=0x${config.meow.workstation.theme.palette.hex.base01}"
            '';
          }))
        ];

        theme = "blahaj";
      };

      loader.timeout = 0;

      consoleLogLevel = 0;
      initrd.verbose = false;

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
