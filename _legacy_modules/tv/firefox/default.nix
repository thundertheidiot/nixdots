{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (lib) mkIf mkMerge mapAttrs;
  inherit (mlib) homeModule;

  firefoxTv = pkgs.wrapFirefox pkgs.firefox-esr-128-unwrapped {
    extraPolicies = {
      DisableAppUpdate = true;
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DontCheckDefaultBrowser = true;

      FirefoxSuggest = {
        WebSuggestions = false;
        SponsoredSuggestions = false;
        ImproveSuggest = false;
      };

      ExtensionSettings =
        mapAttrs (_: v: {
          installation_mode = "force_installed";
          install_url = v;
        })
        {
          # ublock origin
          "uBlock0@raymondhill.net" = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/addon-11423598-latest.xpi";
          # sponsorblock
          "sponsorBlocker@ajay.app" = "https://addons.mozilla.org/firefox/downloads/latest/sponsorblock/addon-13499177-latest.xpi";
          # youtube nonstop
          "{0d7cafdd-501c-49ca-8ebb-e3341caaa55e}" = "https://addons.mozilla.org/firefox/downloads/latest/youtube-nonstop/addon-14307471-latest.xpi";
          # youtube smart tv
          "{d2bcedce-889b-4d53-8ce9-493d8f78612a}" = "https://addons.mozilla.org/firefox/downloads/latest/youtube-for-tv/addon-15327592-latest.xpi";
          # i still don't care about cookies
          "idcac-pub@guus.ninja" = "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/addon-17568914-latest.xpi";
        };
    };
  };

  cfg = config.meow.tv;
in {
  config = mkIf cfg.enable (let
  in
    mkMerge [
      (homeModule {
        programs.firefox = {
          enable = true;

          profiles."nix-managed".id = 0;

          profiles."tv" = {
            id = 1;
          };
        };

        home.file."kodi_launcher.json".text = builtins.toJSON (let
          mkScript = name: url:
            pkgs.writers.writeBash name ''
              echo "==== ${name} ===="
              env

              # ${firefoxTv}/bin/firefox -P tv --kiosk "${url}"
              ${pkgs.firefox}/bin/firefox -P tv --kiosk "${url}"
            '';
        in {
          "Home" =
            mkScript "firefox_tv" "";
          "Areena TV" =
            mkScript "areena_tv" "https://areena.yle.fi";
          "Youtube TV" =
            mkScript "youtube_tv" "https://youtube.com/tv";
        });
      })
    ]);
}
