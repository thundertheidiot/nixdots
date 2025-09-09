{inputs, ...}: {
  imports = [
    inputs.flake-parts.flakeModules.modules
    ./nixos-configurations.nix
    ./pkgs.nix
    ./mksystem.nix
  ];

  systems = [
    "linux-x86_64"
  ];
}
