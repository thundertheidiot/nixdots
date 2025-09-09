{inputs, ...}: {
  imports = [
    inputs.flake-parts.flakeModules.modules
    inputs.actions.flakeModules.default

    ./actions.nix
    ./mksystem.nix
    ./nixos-configurations.nix
    ./pkgs.nix
  ];

  systems = [
    "linux-x86_64"
  ];
}
