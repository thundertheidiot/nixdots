{
  config,
  lib,
  inputs,
  ...
}: let
  inherit (builtins) readDir;
  inherit (lib.strings) removeSuffix;
  inherit (lib) isFunction;
  inherit (lib.attrsets) mapAttrs' hasAttr isAttrs;
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

        fn = isFunction cfg';
        attr = isAttrs cfg';
        hasConfig = attr && hasAttr "config" cfg';
        hasModules = attr && hasAttr "modules" cfg';

        cfg =
          if fn
          then cfg'
          else if hasConfig
          then cfg'.config
          else throw "No configuration.";

        modules =
          if fn
          then []
          else if hasModules
          then cfg'.modules
          else throw "No modules.";
      in {
        inherit cfg modules;
      });
    }) (readDir "${inputs.self.outPath}/hosts");
}
