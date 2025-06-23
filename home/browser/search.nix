{
  "g" = {
    urls = [
      {
        template = "https://google.com/search";
        params = [
          {
            name = "q";
            value = "{searchTerms}";
          }
        ];
      }
    ];
    definedAliases = ["@g"];
  };
  "ddg" = {
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
    definedAliases = ["@ddg"];
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
    definedAliases = ["@np"];
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

    definedAliases = ["@no"];
  };
  "Home Manager Options" = {
    urls = [
      {
        template = "https://home-manager-options.extranix.com";
        params = [
          {
            name = "query";
            value = "{searchTerms}";
          }
        ];
      }
    ];

    definedAliases = ["@hm"];
  };
}
