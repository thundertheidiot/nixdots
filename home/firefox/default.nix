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
        search = {
          force = true;
          default = "DuckDuckGo";
          engines = {
            "DuckDuckGo" = {
              urls = [{
                template = "https://duckduckgo.com/";
                params = [
                  { name = "q"; value = "{searchTerms}"; }
                ];
              }];
            };
          };
        };
      };
    };
  });
}
