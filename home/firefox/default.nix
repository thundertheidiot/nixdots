{
  config,
  pkgs,
  lib,
  localconfig,
  ...
}: {
  config = lib.mkIf (localconfig.install.firefox) (with config; {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox;

      profiles."nix-managed" = {
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          ublock-origin
          purpleadblock
          istilldontcareaboutcookies
          bitwarden
          privacy-redirect
          multi-account-containers
          user-agent-string-switcher
        ];

        containers = {
          youtube = {
            color = "red";
          };
        };

        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
          engines = {
            "DuckDuckGo" = {
              urls = [{
                template = "https://duckduckgo.com/";
                params = [
                  { name = "q"; value = "{searchTerms}"; }
                ];
              }];
            };
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
            };
          };
        };
      };
    };
  });
}
