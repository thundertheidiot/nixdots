{
  config,
  pkgs,
  lib,
  mlib,
  ...
}: let
  inherit (mlib) homeModule mkOpt;
  inherit (lib) mkMerge types;

  # Easier to deal with than my abstraction for this scale
  inherit (lib.options) mkOption;

  inherit (builtins) mapAttrs concatStringsSep;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList;

  mkSubMod = options:
    types.attrsOf (types.submodule ({
      config,
      name,
      ...
    }: {
      inherit options;
    }));

  mkSubMod' = module:
    types.attrsOf (types.submodule module);

  subModOpt = options: attrs:
    mkOption {
      type = mkSubMod options;
    }
    // attrs;

  cfg = config.meow.browser;
in {
  options = {
    meow.browser.firefoxConfig =
      subModOpt {
        configPath = mkOpt types.str ".mozilla/firefox" {
          description = "Path to configuration directory, relative to home directory.";
        };

        profilesPath = mkOpt types.str config.configPath {};

        startWithLastProfile = mkOpt types.bool true {};

        profiles =
          mkOpt (mkSubMod' ({
            config,
            name,
            ...
          }: {
            options = {
              name = mkOpt (types.str) name {
                description = "Profile name.";
              };
              id = mkOpt (types.ints.unsigned) 0 {
                description = "Profile id, set to unique number.";
              };
              path = mkOpt (types.str) config.name {
                description = "Profile path, relative to config dir.";
              };
              default = mkOpt (types.bool) true {
                description = "Set as default profile.";
              };

              userPref = mkOpt (types.attrsOf (pkgs.formats.json {}).type) {} {
                description = "Attrset of Firefox preferences. Json format.";
              };
              extraUserJs = mkOpt (types.str) "" {
                description = "Extra user.js config to be placed at the end.";
              };

              search = {
                force = mkOpt (types.bool) true {
                  description = "Forcefully replace existing search configuration.";
                };
                default = mkOpt (types.str) "DuckDuckGo" {
                  description = "Default search engine.";
                };
                privateDefault = mkOpt (types.str) "DuckDuckGo" {
                  description = "Default search engine for private browsing mode.";
                };

                engines = mkOpt (types.attrsOf (types.attrsOf (pkgs.formats.json {}).type)) {} {
                  description = ''
                    Search engine configuration
                  '';
                  # https://searchfox.org/mozilla-central/rev/669329e284f8e8e2bb28090617192ca9b4ef3380/toolkit/components/search/SearchEngine.jsm#1138-1177
                };
              };
            };
          })) {} {
            description = "Attribute set of profile configurations.";
          };
      } {
        description = "Attrs of config per (fork of) firefox.";
        default = {};
      };
  };

  config = let
    ff = cfg.firefoxConfig;
  in
    homeModule ({config, ...}: {
      home.file =
        mkMerge
        (mapAttrsToList (
            _: ffc:
              {
                "${ffc.configPath}/profiles.ini".text = lib.generators.toINI {} (
                  {
                    General = {
                      StartWithLastProfile =
                        if ffc.startWithLastProfile
                        then 1
                        else 0;
                      Version = 2; # ?
                    };
                  }
                  // (mapAttrs' (n: v: {
                      name = "Profile${toString v.id}";
                      value = {
                        Default = v.default;
                        IsRelative = 1;
                        Name = v.name;
                        Path = v.path;
                      };
                    })
                    ffc.profiles)
                );
              }
              // (mapAttrs' (n: v: {
                  name = "${ffc.profilesPath}/${v.path}/user.js";
                  value = {
                    text = let
                      mkValue = pref:
                        builtins.toJSON (
                          if lib.isBool pref || lib.isInt pref || lib.isString pref
                          then pref
                          else builtins.toJSON pref
                        );
                    in ''
                      // Managed by nix

                      ${concatStringsSep "\n" (
                        mapAttrsToList
                        (n: v: "user_pref(\"${n}\", ${mkValue v});")
                        v.userPref
                      )}

                      ${v.extraUserJs}
                    '';
                  };
                })
                ffc.profiles)
            # TODO: search engines
            # https://github.com/nix-community/home-manager/blob/2f23fa308a7c067e52dfcc30a0758f47043ec176/modules/programs/firefox.nix#L786
            # // (mapAttrs' (n: v: {
            #     name = "${ffc.profilesPath}/${v.path}/user.js";
            #     value = {
            #       force = true;
            #       source = let
            #         settings = {
            #           version = 6;
            #         };
            #         # This is some stupid firefox thing taken from home-manager
            #         disclaimer = appName:
            #           "By modifying this file, I agree that I am doing so "
            #           + "only within ${appName} itself, using official, user-driven search "
            #           + "engine selection processes, and in a way which does not circumvent "
            #           + "user consent. I acknowledge that any attempt to change this file "
            #           + "from outside of ${appName} is a malicious act, and will be responded "
            #           + "to accordingly.";
            #         a = "a";
            #       in
            #         a;
            #     };
            #   })
            # ffc.profiles)
          )
          ff);
    });
}
