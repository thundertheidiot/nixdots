{
  config,
  lib,
  inputs,
  ...
}: let
  inherit (builtins) readDir;
  inherit (lib.strings) removeSuffix;
  inherit (lib) isFunction isList;
  inherit (lib.attrsets) mapAttrs' isAttrs;
in {
  flake.nixosConfigurations = let
    getName = rec {
      regular = name:
        removeSuffix ".nix" name;

      directory = name: name;

      symlink = regular;
      unknown = name: throw "${name} is of file type unknown, aborting";
    };
  in
    mapAttrs' (n: v: {
      name = getName.${v} n;
      value = config.flake.mkSystem (let
        cfg' = import "${inputs.self.outPath}/hosts/${n}";

        justCfg = isFunction cfg' || isAttrs cfg';

        # cfg =
        #   if fn
        #   then cfg'
        #   else if hasConfig
        #   then cfg'.config
        #   else throw "No configuration.";

        modules =
          if justCfg
          then [cfg']
          else if isList cfg'
          then
            config.flake.lib.nixosModules cfg'
            ++ [
              {
                home-manager.sharedModules = config.flake.lib.homeModules cfg';
              }
            ]
          else throw "No modules.";
      in {
        inherit modules;
      });
    }) (readDir "${inputs.self.outPath}/hosts");
}
