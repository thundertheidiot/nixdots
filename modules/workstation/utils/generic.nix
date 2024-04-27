{
  system = {
    config,
    pkgs,
    lib,
    ...
  }:
    lib.mkIf (config.workstation.utils == "generic/gtk") {
      environment.systemPackages = with pkgs; [
        gnome.gnome-keyring
        gparted
      ];

      services.gvfs.enable = true;

      services.gnome.gnome-keyring.enable = true;
      programs.seahorse.enable = true;

      security.pam.services.gnome-keyring = {
        name = "gnome-keyring";
        enableGnomeKeyring = true;
        text = ''
          auth optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so
          session optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so auto_start
          password optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so
        '';
      };

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
    ...
  }:
    lib.mkIf (config.workstation.utils == "generic/gtk") {
      home.packages = with pkgs; [
        gparted
        gnome.seahorse
        gnome.gnome-keyring
        blueberry
        (pkgs.cinnamon.nemo-with-extensions.overrideAttrs (final: prev: {
          extensions = with pkgs.cinnamon; [nemo-fileroller];
        }))
        gnome.file-roller
        nsxiv # image viewer
      ];

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
        defaultApplications = builtins.listToAttrs (builtins.map (mime: {
            name = mime;
            value = ["nsxiv.desktop"];
          })
          config.xdg.desktopEntries.nsxiv.mimeType);
      };

      services.gnome-keyring = {
        enable = true;
        components = ["pkcs11" "secrets" "ssh"];
      };

      services.gpg-agent = {
        enable = true;
        pinentryPackage = pkgs.pinentry-gnome3;
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
    };
}
