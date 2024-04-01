{ config, lib, pkgs, ... }: {
  config = lib.mkIf (config.setup.userMachine.enable) (with config; {
    environment.systemPackages = with pkgs; [
      udev-gothic-nf
      cantarell-fonts
      noto-fonts-color-emoji
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
