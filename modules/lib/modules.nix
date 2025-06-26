{
  lib,
  config,
  ...
}: let
  inherit (lib) isAttrs isList mkForce;
  inherit (lib.lists) filter;
in {
  flake.lib.modules = set: modules:
    assert isAttrs set;
    assert isList modules;
      map (k: set."${k}" or {}) modules;

  # flake.lib.filterImports = {modules}:
  #   assert isList modules; {
  #     # imports = mkForce (filter (i: ) config.imports);
  #   };
}
