{inputs, ...}: {
  imports = [
    inputs.flake-parts.flakeModules.modules
    inputs.actions.flakeModules.default

    ./actions.nix
    ./mksystem.nix
    ./nixos-configurations.nix
    ./pkgs.nix
  ];

  flake.modules = {};

  systems = [
    "x86_64-linux"
  ];

  perSystem = {
    config,
    pkgs,
    ...
  }: {
    devShells.default = pkgs.mkShell {
      packages = with pkgs; [
        just
      ];
    };
  };
}
