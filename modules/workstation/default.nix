let
  modules = [
    (import ./utils/generic.nix)
    (import ./utils/kde.nix)
    (import ./programs/alacritty.nix)
    (import ./programs/gnome-keyring.nix)
    (import ./environment/default.nix)
    (import ./theming.nix)
    (import ./laptop.nix)
    (import ./keyd.nix)
  ];
in {
  system = {
    config,
    pkgs,
    lib,
    mlib,
    inputs,
    ...
  }: {
    imports =
      (mlib.getSystems modules)
      ++ [
        inputs.nix-gaming.nixosModules.pipewireLowLatency
      ];

    config = lib.mkIf (config.workstation.enable) {
      environment.systemPackages = with pkgs; [
        wireguard-tools
        distrobox
        freetube
      ];

      # vpn
      networking.firewall.checkReversePath = false;

      security.polkit.enable = true;

      hardware.bluetooth = {
        enable = true;
        powerOnBoot = true;
        settings = {
          General.Experimental = true;
        };
      };

      # Force disable pulseaudio
      hardware.pulseaudio.enable = lib.mkForce false;

      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        jack.enable = true;

        wireplumber.extraConfig = {
          "monitor.bluez.properties" = {
            "bluez5.enable-hw-volume" = false;
          };
        };

        lowLatency = {
          enable = true;
        };
      };

      # boot.kernelPackages = pkgs.linuxPackages_cachyos-lto;

      services.cpupower-gui.enable = true;

      xdg.portal = {
        enable = true;
        xdgOpenUsePortal = true;

        config.common.default = "";
      };

      services.flatpak.enable = true;

      services.displayManager.sddm = lib.mkMerge [
        (lib.mkIf (config.meow.gpu != "none") {
          enable = true;
          wayland = {
            enable = true;
            compositor = "kwin";
          };
        })
        # TODO nvidia
      ];

      systemd.services."NetworkManager-wait-online".enable = false;
    };
  };

  home = {
    config,
    pkgs,
    lib,
    mlib,
    ...
  }: {
    imports = mlib.getHomes modules;

    config =
      lib.mkIf config.workstation.enable
      {
        home.packages = with pkgs; [
          mpc-cli
          libnotify

          # cinny-desktop
          element-desktop
          signal-desktop

          libreoffice

          gimp
          godot_4

          speedcrunch

          yle-dl
          yt-dlp
          python3

          # ansel

          qmk

          # (pkgs."2311".blender)
          blender

          obs-studio
          kdePackages.kdenlive

          (gajim.overrideAttrs (prev: {
            nativeBuildInputs = prev.nativeBuildInputs ++ [pkgs.makeWrapper];

            # fix gnome-keyring on kde
            postInstall =
              prev.postInstall
              + ''
                wrapProgram $out/bin/gajim --set XDG_CURRENT_DESKTOP GNOME
              '';
          }))

          (mumble.overrideAttrs (prev: {
            postFixup =
              builtins.replaceStrings
              ["wrapProgram $out/bin/mumble"]
              ["wrapProgram $out/bin/mumble --set QT_QPA_PLATFORM xcb"] # Run with xwayland to make keybindings work
              
              prev.postFixup;
          }))
        ];

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

        services.mpd = {
          enable = true;
          musicDirectory = "${config.xdg.userDirs.music}/mpd";
          network.listenAddress = "any";
          extraConfig = ''
            restore_paused "yes"

            audio_output {
                         type "pipewire"
                         name "pipewire"
            }
          '';
        };

        services.mpdris2 = {
          enable = true;
          mpd = {
            host = "127.0.0.1";
            port = 6600;
            musicDirectory = config.services.mpd.musicDirectory;
          };
          # multimediaKeys = true;
          notifications = false;
        };

        programs.ncmpcpp = {
          enable = true;
          settings = {
            ncmpcpp_directory = "${config.xdg.dataHome}/ncmpcpp";
            lyrics_directory = "${config.xdg.dataHome}/ncmpcpp/lyrics";
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
            hwdec = "auto-safe";
            vo = "gpu";
            profile = "gpu-hq";
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
      };
  };
}
