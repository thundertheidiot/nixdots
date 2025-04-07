{...}: {
  config = {
    meow = {
      browser.enable = ["firedragon"];

      browser.firefoxConfig.firedragon = {
        policies = {
          "idcac-pub@guus.ninja" = "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/addon-17568914-latest.xpi";
          "{446900e4-71c2-419f-a6a7-df9c091e268b}" = "https://addons.mozilla.org/firefox/downloads/latest/bitwarden-password-manager/addon-12533945-latest.xpi";
        };

        profiles."nix-managed" = {
          id = 0;
          default = true;

          userPref = {
            "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;

            # privacy
            "network.trr.mode" = 3;
            "network.trr.uri" = "https://dns.quad9.net/dns-query";

            # these break websites unfortunately
            "privacy.resistFingerprinting" = false;
            "webgl.disabled" = false;

            # ui
            "ui.systemUsesDarkTheme" = 1;
            "floorp.browser.user.interface" = 1;
            "floorp.chrome.theme.mode" = 1;
          };

          search = {
            force = true;
            default = "DuckDuckGo";
            privateDefault = "DuckDuckGo";
            engines = {
              "DuckDuckGo" = {
                urls = [
                  {
                    template = "https://duckduckgo.com/";
                    params = [
                      {
                        name = "q";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
              };
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
              };
              "Nix Options" = {
                urls = [
                  {
                    template = "https://search.nixos.org/options";
                    params = [
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
              };
              "Home Manager Options" = {
                urls = [
                  {
                    template = "https://home-manager-options.extranix.com/";
                    params = [
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
              };
            };
          };
        };
      };
    };
  };
}
