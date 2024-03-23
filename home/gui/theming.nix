{ config, pkgs, ... }: let
  cursor_package = pkgs.whitesur-cursors;
  cursor_name = "WhiteSur Cursors";
in with config; {
  gtk.cursorTheme = {
    package = cursor_package;
    name = cursor_name;
    size = 16;
  };
  
  home.pointerCursor = {
    package = cursor_package;
    name = cursor_name;
    size = 16;
    
    x11.defaultCursor = "left_ptr";
    x11.enable = true;
  };
}
