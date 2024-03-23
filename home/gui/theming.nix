{ config, pkgs, ... }: let
  cursor_package = pkgs.whitesur-cursors;
  cursor_name = "WhiteSur Cursors";
in with config; {
  home.packages = with pkgs; [
    jetbrains-mono
    meslo-lgs-nf
  ];

  fonts.fontconfig.enable = true;

  home.file.".config/fontconfig/fonts.conf".text =
    ''
<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
<alias>
<family>sans-serif</family>
<prefer>
<family>Noto Sans</family>
</prefer>
</alias>
  
<alias>
<family>serif</family>
<prefer>
<family>Noto Serif</family>
</prefer>
</alias>

<alias>
<family>monospace</family>
<prefer>
<family>JetBrainsMono Nerd Font</family>
<family>JetBrainsMono NFM</family>
</prefer>
</alias>
</fontconfig>
'';

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
