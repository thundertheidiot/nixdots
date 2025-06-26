{
  config,
  lib,
  inputs,
  ...
}: let
  inherit (builtins) readDir;
  inherit (lib.strings) removeSuffix;
  inherit (lib.attrsets) mapAttrs';
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

        # modules = cfg'.modules;
        # cfg = cfg'.config;
        cfg = cfg';
        modules = [];
      in {
        inherit cfg modules;
      });
    }) (readDir "${inputs.self.outPath}/hosts");
}
