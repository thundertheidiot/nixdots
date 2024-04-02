{ config, lib, pkgs, ... }: {
  config = lib.mkIf (config.setup.userMachine.enable) (with config; {
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
  });
}
