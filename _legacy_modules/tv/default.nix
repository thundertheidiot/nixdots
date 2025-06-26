{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkEnOpt homeModule;
  inherit (lib) mkIf mkMerge;

  cfg = config.meow.tv;
in {
  options = {
    meow.tv.enable = mkEnOpt "Enable smart-tv configuration";
  };

  imports = [
    ./settings
    ./firefox
  ];

  config = mkIf cfg.enable (mkMerge [
    # kodi settings
    {
      meow.tv.kodiSettings = {
        enable = true;
        addons = [
          "plugin.video.jellyfin"
          "plugin.video.youtube"
          "script.firefox.launcher"
          "script.module.inputstreamhelper"
        ];
        settings = let
          # xml is super weird
          mkSetting = id: val: {
            "@id" = id;
            "@default" = "false";
            "#text" = toString val;
          };
        in {
          "userdata/guisettings.xml" = {
            settings = {
              "@version" = "2";
              setting = [
                # (mkSetting "lookandfeel.skin" "skin.estuary.modv2")
                (mkSetting "locale.timezonecountry" "Finland")
                (mkSetting "locale.timezone" "Europe/Helsinki")
                (mkSetting "locale.use24hourclock" true)
                (mkSetting "general.addonupdates" 2)
              ];
            };
          };

          "userdata/addon_data/plugin.video.jellyfin/settings.xml" = {
            settings = {
              "@version" = "2";
              setting = [
              ];
            };
          };
        };
      };
    }
    # kodi cage
    {
      users.extraUsers."cage".isNormalUser = true;

      services.cage = {
        enable = true;
        user = "cage";
        program = "${pkgs.callPackage ./kodi {}}/bin/kodi";
      };
    }
    # home manager
    {
      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
        backupFileExtension = "hm_backup";

        users."cage" = {};
      };
    }
    # web interface ports
    {
      networking.firewall = {
        allowedTCPPorts = [8080];
        allowedUDPPorts = [8080];
      };
    }
    # widevine
    {
      home-manager.users."cage".imports = [
        {
          home.file = {
            widevine-lib = {
              source = "${pkgs.widevine-cdm}/share/gogle/chrome/WidevineCdm/_platform_specific/linux_x64/libwidevinecdm.so";
              target = ".kodi/cdm/libwidevinecdm.so";
            };
            widevine-manifest = {
              source = "${pkgs.widevine-cdm}/share/gogle/chrome/WidevineCdm/manifest.json";
              target = ".kodi/cdm/manifest.json";
            };
          };
        }
      ];
    }
    # plymouth
    {
      boot = {
        plymouth = {
          enable = true;

          themePackages = with pkgs; [
            plymouth-blahaj-theme
          ];

          theme = "blahaj";
        };

        loader.timeout = 0;

        consoleLogLevel = 0;
        initrd.verbose = false;

        kernelParams = [
          "quiet"
          "splash"
          "boot.shell_on_fail"
          "loglevel=3"
          "rd.systemd.show_status=false"
          "rd.udev.log_level=3"
          "udev.log_priority=3"
        ];
      };
    }
  ]);
}
