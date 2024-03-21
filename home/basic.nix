{ config, pkgs, inputs, ... }: {
  config = {
    home.file.".config/wget/wgetrc" = {
      text = "hsts-file = \"$XDG_CACHE_HOME\"/wget-hsts";
    };

    programs.fish = {
      enable = true;
      shellAliases = {
        "m" = "mpv --no-video --loop=yes";
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

        hints.enabled = {
          command = "xdg-open";
          hyperlinks = true;
          post_processing = true;
          regex = "(ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\u0000-\u001F\u007F-<>\"\\s{-}\\^⟨⟩`]+";

          mouse = {
            enabled = true;
            mods = "None";
          };
        };

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

        mouse.hide_while_typing = true;

        colors = with config.scheme.withHashtag;
          let
            default = {
              black = base00; white = base07;
              inherit red green yellow blue cyan magenta;
            };
          in {
            primary = { background = base00; foreground = base07; };
            cursor = { text = base02; cursor = base07; };
            normal = default; bright = default; dim = default;
          };
      };

    };
  };
}
