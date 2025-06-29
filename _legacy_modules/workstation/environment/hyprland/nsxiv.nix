{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (lib) mkIf;
  inherit (builtins) elem;

  work = config.meow.workstation.enable;
  env = config.meow.workstation.environment;
in {
  config = mkIf (work && elem "hyprland" env) {
    environment.systemPackages = [pkgs.nsxiv];

    programs.fish.shellAliases = {
      "sxiv" = "nsxiv";
    };

    meow.home.modules = [
      ({config, ...}: {
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
      })
    ];
  };
}
