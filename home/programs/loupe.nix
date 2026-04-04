{
  pkgs,
  lib,
  ...
}: let
  inherit (lib) concatMapAttrs mergeAttrsList genAttrs;
in {
  config = {
    home.packages = [pkgs.loupe];

    xdg.mime.enable = true;

    xdg.mimeApps = {
      enable = true;
      defaultApplications =
        concatMapAttrs
        (app: list: genAttrs list (_: [app]))
        {
          "org.gnome.Loupe.desktop" = [
            "application/postscript"
            "image/avif"
            "image/bmp"
            "image/gif"
            "image/gp2"
            "image/heic"
            "image/heif"
            "image/jpeg"
            "image/jpg"
            "image/jxl"
            "image/pjpeg"
            "image/png"
            "image/svg+xml"
            "image/tiff"
            "image/webp"
            "image/x-bmp"
            "image/x-png"
            "image/x-tga"
            "image/x-xpixmap"
          ];
        };
    };
  };
}
