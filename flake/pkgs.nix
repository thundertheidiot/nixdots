{ lib, withSystem, ... }:
let
  inherit (lib) mapAttrs;
in
{
  perSystem = { pkgs, ... }: {
    packages = import ../pkgs { inherit pkgs; };
  };

  # custom packages get stable nix-update by default
  # passthru.autoUpdate can add args, supply a script or explain a skip
  flake.packageUpdates = withSystem "x86_64-linux" (
    { pkgs, ... }:
    mapAttrs (
      _: package:
      let
        policy = package.autoUpdate or { };
      in
      {
        args = policy.args or [ ];
        skip = policy.skip or null;
        script = policy ? script;
      }
    ) (import ../pkgs { inherit pkgs; })
  );
}
