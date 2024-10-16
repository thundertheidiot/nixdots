# TODO: move under workstation
{
  system = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.enable) {
    };

  home = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.enable) {
      home.packages = with pkgs; [
        seahorse
        gnome-keyring
      ];

    };
}
