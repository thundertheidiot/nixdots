{
  system = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.utils == "generic/gtk") {
      environment.systemPackages = with pkgs; [
        gparted
      ];

      services.gvfs.enable = true;

      systemd.user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = ["graphical-session.target"];
        wants = ["graphical-session.target"];
        after = ["graphical-session.target"];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    };

  home = {
    config,
    pkgs,
    lib,
    mlib,
    ...
  }:
    lib.mkIf (config.workstation.utils == "generic/gtk") (
      let
        colors = mlib.colors config;
      in {
        home.packages = with pkgs; [
          gparted
          blueberry
          (pkgs.nemo-with-extensions.overrideAttrs (final: prev: {
            extensions = with pkgs; [nemo-fileroller];
          }))
          file-roller
          nsxiv # image viewer
        ];

        xresources = {
          path = "${config.xdg.configHome}/xresources";
          properties = {
            "Nsxiv.window.background" = "${colors.background}";
            "Nsxiv.window.foreground" = "${colors.foreground}";
            "Nsxiv.mark.foreground" = "${colors.base04}";

            "Nsxiv.bar.background" = "${colors.foreground}";
            "Nsxiv.bar.foreground" = "${colors.background}";
          };
        };

        programs.fish.shellAliases = {
          "sxiv" = "nsxiv";
        };

        xdg.mime.enable = true;
        xdg.desktopEntries.nsxiv = {
          name = "nsxiv";
          type = "Application";
          genericName = "Image Viewer";
          noDisplay = true;
          exec = "${pkgs.nsxiv}/bin/nsxiv %F";
          mimeType = [
            "image/bmp"
            "image/gif"
            "image/jpeg"
            "image/jpg"
            "image/png"
            "image/tiff"
            "image/x-bmp"
            "image/x-portable-anymap"
            "image/x-portable-bitmap"
            "image/x-portable-graymap"
            "image/x-tga"
            "image/x-xpixmap"
            "image/webp"
            "image/heic"
            "image/svg+xml"
            "application/postscript"
            "image/gp2"
            "image/jxl"
            "image/avif"
            "image/heif"
          ];
        };
        xdg.mimeApps = {
          enable = true;
          defaultApplications = builtins.listToAttrs (builtins.map
            (mime: {
              name = mime;
              value = ["nsxiv.desktop"];
            })
            config.xdg.desktopEntries.nsxiv.mimeType);
        };
      }
    );
}
