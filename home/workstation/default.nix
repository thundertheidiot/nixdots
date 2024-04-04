{
  config,
  pkgs,
  lib,
  ...
}: let
  xwayland-mumble = pkgs.writeShellScriptBin "mumble" ''
    # This allows global keybindings in mumble, they don't work with the wayland backend for some reason
    export QT_QPA_PLATFORM=xcb
    ${pkgs.mumble}/bin/mumble
  '';
in {
  imports = [
    ./theming.nix
    ./gaming
  ];

  config = lib.mkIf (config.setup.userMachine.enable) (with config; {
    home.packages = with pkgs; [
      xwallpaper
      xclip
      ncmpcpp
      mpc-cli
      libnotify

      gajim
      cinny-desktop
      element-desktop

      gnome.seahorse
      gnome.gnome-keyring
      gimp
      blueberry
      (pkgs.cinnamon.nemo-with-extensions.overrideAttrs (final: prev: {
        extensions = with pkgs.cinnamon; [ nemo-fileroller ];
      }))
      gnome.file-roller
      nsxiv # image viewer
      speedcrunch # calculator

      xwayland-mumble # above
      obs-studio

      yle-dl
      yt-dlp
      freetube
      python3
      # These don't work yet
      # (import ./packages/sibs.nix { inherit pkgs lib; })
      # (import ./packages/quickmedia.nix { inherit pkgs lib; })
    ];

    xdg.mime.enable = true;
    xdg.desktopEntries.nsxiv = {
      name = "nsxiv";
      mimeType = [ "image/*" ];
    };
    xdg.mimeApps = {
      enable = true;
      defaultApplications = {
        "image/*" = [ "nsxiv.desktop" ];
      };
    };

    xdg.userDirs = {
      enable = true;
      createDirectories = true;
      documents = "${home.homeDirectory}/Documents";
      download = "${home.homeDirectory}/Downloads";
      music = "${home.homeDirectory}/Music";
      pictures = "${home.homeDirectory}/Pictures";
      videos = "${home.homeDirectory}/Videos";
      desktop = "${home.homeDirectory}/.local/share/xdg-dirs/desktop";
      publicShare = "${home.homeDirectory}/.local/share/xdg-dirs/publicshare";
      templates = "${home.homeDirectory}/.local/share/xdg-dirs/templates";
    };

    services.mpd = {
      enable = true;
      musicDirectory = "${xdg.userDirs.music}/mpd";
      network.listenAddress = "any";
      extraConfig = ''
        restore_paused "yes"

        audio_output {
                     type "pipewire"
                     name "pipewire"
        }
      '';
    };

    programs.ncmpcpp = {
      enable = true;
      settings = {
        ncmpcpp_directory = "${xdg.dataHome}/ncmpcpp";
        lyrics_directory = "${xdg.dataHome}/ncmpcpp/lyrics";
        media_library_primary_tag = "album_artist";
        # media_library_split_by_date = "no";
        media_library_hide_album_dates = "yes";
        mpd_host = "127.0.0.1";
        mpd_port = "6600";
      };
      bindings = [
        {
          key = "h";
          command = "previous_column";
        }
        {
          key = "l";
          command = "next_column";
        }
        {
          key = "j";
          command = "scroll_down";
        }
        {
          key = "k";
          command = "scroll_up";
        }

        {
          key = "=";
          command = "volume_up";
        }
        {
          key = "-";
          command = "volume_down";
        }

        {
          key = "y";
          command = "toggle_single";
        }
        {
          key = "u";
          command = "update_database";
        }
      ];
    };

    programs.mpv = {
      enable = true;
      config = {
        volume = 75;
        ytdl-format = "bestvideo[height<=1080]+bestaudio";
        hwdec="auto-safe";
        vo="gpu";
        profile="gpu-hq";
      };
      scripts = with pkgs.mpvScripts; [
        sponsorblock-minimal
        quality-menu
      ];
      bindings = {
        "G" = "script-binding quality_menu/video_formats_toggle";
        "Alt+g" = "script-binding quality_menu/audio_formats_toggle";
        "Ctrl+r" = "script-binding quality_menu/reload";
      };
    };

    services.gnome-keyring = {
      enable = true;
      components = ["pkcs11" "secrets" "ssh"];
    };

    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    programs.qutebrowser = {
      enable = true;
      loadAutoconfig = false;
      searchEngines = {
        ddg = "https://duckduckgo.com/?q={}";
        np = "https://search.nixos.org/packages?channel=unstable&query={}";
        no = "https://search.nixos.org/options?channel=unstable&query={}";
        hm = "https://home-manager-options.extranix.com/?query={}&release=master";
      };
      keyBindings = {
        normal = {
          "=" = "zoom-in";
          "-" = "zoom-out";

          " v" = "hint links spawn mpv {hint-url}";
        };
      };
      settings = {
        content.javascript.enabled = true;
        scrolling.smooth = false;
        content.blocking = {
          method = "adblock";
          adblock.lists = [
            "https://easylist.to/easylist/easylist.txt"
            "https://easylist.to/easylist/easyprivacy.txt"
            "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt"
            "https://easylist.to/easylist/fanboy-social.txt"
            "https://secure.fanboy.co.nz/fanboy-annoyance.txt"
          ];
        };
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
  });
}
