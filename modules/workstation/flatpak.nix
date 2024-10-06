{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt mkEnOptTrue;
  inherit (lib) mkIf;

  cfg = config.meow.workstation.flatpak;
in {
  options = {
    meow.workstation.flatpak.enable = mkEnOptTrue "Enable flatpaks";
    meow.workstation.flatpak.graphicalStore = mkEnOpt "Enable flatpaks";
  };

  # FIXME search broken, install broken
  config = mkIf cfg.enable (lib.mkMerge [
    {
      services.flatpak.enable = true;

      systemd.services.flathub-repo = {
        wantedBy = ["multi-user.target"];
        path = [pkgs.flatpak];
        script = ''
          flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
        '';
      };
    }
    (mkIf cfg.graphicalStore {
      environment.systemPackages = [pkgs.gnome-software];
    })
  ]);
}
