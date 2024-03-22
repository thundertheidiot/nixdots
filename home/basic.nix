{ config, pkgs, localconfig, inputs, ... }: {
  config = {
    home.file.".config/wget/wgetrc" = {
      text = "hsts-file = \"$XDG_CACHE_HOME\"/wget-hsts";
    };

    services.mpd = {
      enable = true;
      musicDirectory = "~/Music/mpd";
      # playlistDirectory = ~/.config/mpd/playlists;
      # network.port = 6600;
    };

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

    programs.fish = {
      enable = true;
      functions = {
        fish_prompt =
          ''
          echo (set_color purple)$USER(set_color normal)'@'(set_color blue)(uname -n)(set_color normal) (pwd) '> '
          '';
          #echo (set_color blue)(uname -n)(set_color normal) (echo $(pwd) | sed 's/$HOME/~/g') '> '
      };
      shellAliases = {
        "m" = "mpv --no-video --loop=yes";
        "e" = "setsid -f emacsclient -c";
      };
    };

    programs.alacritty = {
      enable = true;

      settings = {
        font = {
          normal = {
            family = "monospace";
            style = "Regular";
          };
          bold = {
            family = "monospace";
            style = "Bold";
          };
          italic = {
            family = "monospace";
            style = "Italic";
          };
          bold_italic = {
            family = "monospace";
            style = "Bold Italic";
          };

          size = 9.0;
        };

        cursor = {
          style = {
            shape = "Beam";
            blinking = "Off";
          };
        };

        hints.enabled = [
          {
            command = "xdg-open";
            hyperlinks = true;
            post_processing = true;
            #regex = "(ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\u0000-\u001F\u007F-<>\"\\s{-}\\^⟨⟩`]+";
          }
        ];

        # hints.enabled.mouse = {
        #   enabled = true;
        #   mods = "None";
        # };

        keyboard.bindings = [
          {
            action = "Paste";
            key = "V";
            mods = "Alt";
          }
          {
            action = "Copy";
            key = "C";
            mods = "Alt";
          }
          {
            action = "IncreaseFontSize";
            key = "Equals";
            mods = "Control";
          }
          {
            action = "DecreaseFontSize";
            key = "Minus";
            mods = "Control";
          }
          {
            action = "ResetFontSize";
            key = "Key0";
            mods = "Control";
          }
        ];

        # mouse.hide_while_typing = true;

        colors = with config.scheme;
          let
            default = {
              black = "0x${base00}";
              white = "0x${base07}";
              red = "0x${red}";
              green = "0x${green}";
              yellow = "0x${yellow}";
              blue = "0x${blue}";
              cyan = "0x${cyan}";
              magenta = "0x${magenta}";
            };
          in {
            primary = { background = "0x${base00}"; foreground = "0x${base07}"; };
            cursor = { text = "0x${base02}"; cursor = "0x${base07}"; };
            normal = default; bright = default; dim = default;
          };
      };

    };
  };
}
