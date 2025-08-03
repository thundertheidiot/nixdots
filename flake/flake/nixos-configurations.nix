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
        cfg = import "${inputs.self.outPath}/hosts/${n}";
      in {
        # take in all defined nixos modules, for example nixpkgs overrides are defined in flake.parts
        modules = [cfg] ++ builtins.attrValues config.flake.modules.nixos;
      });
    }) (readDir "${inputs.self.outPath}/hosts");
}
