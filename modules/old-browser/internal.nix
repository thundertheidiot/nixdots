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

  inherit (builtins) concatStringsSep attrValues;
  inherit (lib.attrsets) mapAttrs' mapAttrsToList;

  mkSubMod = module:
    types.attrsOf (types.submodule module);

  subModOpt = options: attrs:
    mkOption {
      type = mkSubMod options;
    }
    // attrs;

  cfg = config.meow.old-browser;
in {
  options = {
    meow.old-browser.firefoxConfig =
      subModOpt ({config, ...}: {
        options = {
          configPath = mkOpt types.str ".mozilla/firefox" {
            description = "Path to configuration directory, relative to home directory.";
          };

          package = mkOpt types.package pkgs.firefox {};

          profilesPath = mkOpt types.str config.configPath {};

          policies = mkOpt (types.attrsOf (pkgs.formats.json {}).type) {} {
            description = "[See list of policies](https://mozilla.github.io/policy-templates/)";
          };

          startWithLastProfile = mkOpt types.bool true {};

          profiles =
            subModOpt ({
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
                  order =
                    mkOpt (types.uniq (types.listOf types.str)) [] {
                    };
                };
              };
            }) {
              description = "Attribute set of profile configurations.";
            };
        };
      }) {
        description = "Attrs of config per (fork of) firefox.";
        default = {};
      };
  };

  config = let
    ff = cfg.firefoxConfig;
  in
    homeModule ({config, ...}: {
      home.packages = map (c:
        c.package.override (old: {
          extraPolicies = (old.extraPolicies or {}) // c.policies;
        })) (attrValues ff);

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
              // (mapAttrs' (n: v: {
                  name = "${ffc.profilesPath}/${v.path}/search.json.mozlz4";
                  value = {
                    force = v.search.force;
                    source = pkgs.callPackage ./engines.nix {
                      path = "${ffc.profilesPath}/${v.path}";
                      inherit (v) name;
                      inherit (v.search) engines default privateDefault order;
                    };
                  };
                })
                ffc.profiles)
            # TODO: rest of hm functionality
          )
          ff);
    });
}
