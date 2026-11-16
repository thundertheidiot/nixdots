{ inputs, ... }: {
  imports = [
    inputs.flake-parts.flakeModules.modules
    inputs.actions.flakeModules.default

    ./actions.nix
    ./mksystem.nix
    ./nixos-configurations.nix
    ./pkgs.nix
    ./lib
  ];

  flake.modules = { };

  systems = [
    "x86_64-linux"
  ];

  perSystem =
    {
      config,
      pkgs,
      ...
    }:
    let
      # nix 2.35 flake metadata no longer copies the flake to the store, prefetch does
      nix-update = pkgs.nix-update.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace nix_update/options.py \
            --replace-fail '["nix", "flake", "metadata", "--json"]' '["nix", "flake", "prefetch", "--json", "."]' \
            --replace-fail 'metadata.get("path")' 'metadata.get("storePath")'
        '';
      });
    in
    {
      devShells.default = pkgs.mkShell {
        # shellHook = ''
        #   ${config.pre-commit.installationScript}
        # '';

        packages = with pkgs; [
          just
          cachix
          gh
          nix-update
          jq
          nh
          nixos-rebuild
        ];
      };

      devShells.genkeys = pkgs.mkShell {
        packages = with pkgs; [
          config.packages.wireguard-keys
          sops
          wireguard-tools
          age
          qrencode
          jq
        ];
      };

    };
}
