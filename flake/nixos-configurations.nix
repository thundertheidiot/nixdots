{
  config,
  lib,
  inputs,
  ...
}:
let
  inherit (builtins) readDir;
  inherit (lib.strings) removeSuffix;
  inherit (lib.attrsets) mapAttrs' filterAttrs;
in
{
  flake.nixosConfigurations =
    let
      getName = rec {
        regular = name: removeSuffix ".nix" name;

        directory = name: name;

        symlink = regular;
        unknown = name: throw "${name} is of file type unknown, aborting";
      };
    in
    mapAttrs' (n: v: {
      name = getName.${v} n;
      value = config.flake.mkSystem (
        let
          cfg = import "${inputs.self.outPath}/hosts/${n}";
        in
        {
          modules = [ cfg ];
        }
      );
    }) (filterAttrs (name: type:
      (type == "directory" && builtins.pathExists "${inputs.self.outPath}/hosts/${name}/default.nix")
      || lib.elem name [ "iso.nix" "x220.nix" "digiboksi.nix" ]
    ) (readDir "${inputs.self.outPath}/hosts"));
}
