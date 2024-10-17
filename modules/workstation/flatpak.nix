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
    meow.workstation.flatpak.enable = mkEnOpt "Enable flatpaks";
    meow.workstation.flatpak.graphicalStore = mkEnOpt "Enable store for flatpaks";
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
    # {
    #   system.fsPackages = [pkgs.bindfs];
    #   fileSystems = let
    #     mkRoSymBind = path: {
    #       device = path;
    #       fsType = "fuse.bindfs";
    #       options = ["ro" "resolve-symlinks" "x-gvfs-hide"];
    #     };
    #     aggregatedIcons = pkgs.buildEnv {
    #       name = "system-icons";
    #       paths = [
    #         config.meow.workstation.theming.iconTheme.package
    #       ];
    #       pathsToLink = ["/share/icons"];
    #     };
    #     aggregatedFonts = pkgs.buildEnv {
    #       name = "system-fonts";
    #       paths = config.fonts.packages;
    #       pathsToLink = ["/share/fonts"];
    #     };
    #   in {
    #     "/usr/share/icons" = mkRoSymBind "${aggregatedIcons}/share/icons";
    #     "/usr/local/share/fonts" = mkRoSymBind "${aggregatedFonts}/share/fonts";
    #   };

    #   meow.workstation.waybarDiskFilter = [
    #     "/usr/share/icons"
    #     "/usr/local/share/fonts"
    #   ];
    # }
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
    (mkIf cfg.graphicalStore {
      environment.systemPackages = [pkgs.gnome-software];
    })
  ]);
}
