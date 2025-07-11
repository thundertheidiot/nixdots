{
  lib,
  config,
  ...
}: let
  inherit (lib) isAttrs isList isString mkForce;
  inherit (lib.lists) filter;
in {
  flake.lib.onlyFlakeModules = set: modules:
    map (m: set."${m}" or {}) (filter isString modules);

  flake.lib.getModules = set: modules:
    assert isAttrs set;
    assert isList modules;
      map (m:
        if isString m
        then set."${m}" or {}
        else m)
      modules;

  flake.lib.nixosModules = l: config.flake.lib.getModules config.flake.modules.nixos l;
  flake.lib.homeModules = l: config.flake.lib.onlyFlakeModules config.flake.modules.homeManager l;

  # flake.lib.filterImports = {modules}:
  #   assert isList modules; {
  #     # imports = mkForce (filter (i: ) config.imports);
  #   };
}
