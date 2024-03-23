{
  config,
  pkgs,
  inputs,
  ...
}:
with config; {
  imports = [
    ./theming.nix
  ];

  home.packages = with pkgs; [
    xwallpaper
    xclip
    wl-clipboard
    gajim
    gnome.seahorse
    gnome.gnome-keyring
  ];

  services.gnome-keyring = {
    enable = true;
    components = ["pkcs11" "secrets" "ssh"];
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    documents = "${config.home.homeDirectory}/Documents";
    download = "${config.home.homeDirectory}/Downloads";
    music = "${config.home.homeDirectory}/Music";
    pictures = "${config.home.homeDirectory}/Pictures";
    videos = "${config.home.homeDirectory}/Videos";
    desktop = "${config.home.homeDirectory}/.local/share/xdg-dirs/desktop";
    publicShare = "${config.home.homeDirectory}/.local/share/xdg-dirs/publicshare";
    templates = "${config.home.homeDirectory}/.local/share/xdg-dirs/templates";
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

      colors = with config.scheme; let
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
        primary = {
          background = "0x${base00}";
          foreground = "0x${base07}";
        };
        cursor = {
          text = "0x${base02}";
          cursor = "0x${base07}";
        };
        normal = default;
        bright = default;
        dim = default;
      };
    };
  };
}
