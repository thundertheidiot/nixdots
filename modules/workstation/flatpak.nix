{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt;
  inherit (lib) mkIf;

  cfg = config.meow.workstation.flatpak;
in {
  options = {
    meow.workstation.flatpak.enable = mkEnOpt "Enable flatpaks";
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
    {
      # Impermanence configuration
      meow.impermanence.directories = let
        cfg = config.meow.impermanence;
      in [
        {
          path = "/var/lib/flatpak";
          persistPath = "${cfg.persist}/flatpak";
          permissions = "755";
        }
      ];

      systemd.tmpfiles.rules = [
        "R! /var/tmp/flatpak-cache-* - - - 10d"
      ];
    }
  ]);
}
