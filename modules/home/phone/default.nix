{ lib, pkgs, config, ... }: {
  config = lib.mkIf (config.setup.phone) {
    home.packages = with pkgs; [
      maliit-keyboard
      maliit-framework
    ];
  };
}
