{inputs, ...}: {
  imports = [
    inputs.flake-parts.flakeModules.modules
    inputs.actions.flakeModules.default

    ./actions.nix
    ./mksystem.nix
    ./nixos-configurations.nix
    ./pkgs.nix
    ./lib
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
      # shellHook = ''
      #   ${config.pre-commit.installationScript}
      # '';

      packages = with pkgs; [
        just
        cachix
      ];
    };

    devShells.genkeys = pkgs.mkShell {
      packages = with pkgs; [
        (sbcl.withPackages
          (ps: with ps; [shasht]))
      ];
    };
  };
}
