{
  lib,
  config,
  inputs,
  ...
}
: let
  inherit (lib) isAttrs isFunction isList evalModules mkOption;
  inherit (lib.types) listOf str;

  inherit (config.flake) root;
in {
  flake.mkSystem = {modules}:
    assert isList modules;
    # TODO think about this crazy hack
    # (let
    #   modulesPath = "${inputs.nixpkgs}/nixos/modules";
    #   baseModules =
    #     import "${modulesPath}/module-list.nix"
    #     ++ [
    #       {
    #         options.modules = mkOption {
    #           type = listOf str;
    #           default = [];
    #         };
    #       }
    #       cfg
    #       "${root}/sops"
    #       inputs.home-manager.nixosModules.home-manager
    #       inputs.chaotic.nixosModules.default
    #       inputs.sops-nix.nixosModules.default
    #       inputs.disko.nixosModules.default
    #       inputs.hyprland.nixosModules.default
    #       inputs.stylix.nixosModules.stylix
    #       inputs.authentik-nix.nixosModules.default
    #       inputs.nix-gaming.nixosModules.pipewireLowLatency
    #       ({lib, ...}: {
    #         imports = import "${root}/_legacy_modules";
    #         nixpkgs = {
    #           overlays = [
    #             inputs.emacs-overlay.overlay
    #             inputs.rust-overlay.overlays.default
    #             inputs.nixpkgs-xr.overlays.default
    #             (import "${root}/overrides.nix" {inherit lib inputs;})
    #           ];
    #         };
    #         home-manager = {
    #           extraSpecialArgs = {inherit inputs mlib;};
    #           sharedModules = [
    #             inputs.plasma-manager.homeManagerModules.plasma-manager
    #             inputs.hyprlux.homeManagerModules.default
    #           ];
    #         };
    #       })
    #     ];
    #   # legacy lib
    #   mlib = import "${root}/lib" {inherit lib;};
    #   specialArgs = {inherit inputs mlib modulesPath;};
    #   extractedConfig = evalModules {
    #     class = "nixos";
    #     inherit specialArgs;
    #     modules =
    #       baseModules
    #       ++ [
    #         cfg
    #       ];
    #   };
    #   extractedModules = config.flake.lib.modules config.flake.modules.nixos extractedConfig.config.modules;
    # in
    #   evalModules {
    #     class = "nixos";
    #     inherit specialArgs;
    #     modules =
    #       baseModules
    #       ++ extractedModules
    #       ++ [
    #         cfg
    #       ];
    #   })
      lib.nixosSystem (let
        # legacy lib
        mlib = import "${root}/lib" {inherit lib;};
      in {
        specialArgs = {
          inherit inputs mlib;
        };
        modules =
          modules
          ++ [
            "${root}/sops"
            inputs.home-manager.nixosModules.home-manager
            inputs.chaotic.nixosModules.default
            inputs.sops-nix.nixosModules.default
            inputs.disko.nixosModules.default
            inputs.hyprland.nixosModules.default
            inputs.stylix.nixosModules.stylix
            inputs.authentik-nix.nixosModules.default
            inputs.nix-gaming.nixosModules.pipewireLowLatency
            ({lib, ...}: {
              imports = import "${root}/_legacy_modules";
              nixpkgs = {
                overlays = [
                  inputs.emacs-overlay.overlay
                  inputs.rust-overlay.overlays.default
                  inputs.nixpkgs-xr.overlays.default
                  # (import "${root}/overrides.nix" {inherit lib inputs;})
                ];
              };
              home-manager = {
                extraSpecialArgs = {inherit inputs mlib;};
                sharedModules = [
                  inputs.plasma-manager.homeManagerModules.plasma-manager
                  inputs.hyprlux.homeManagerModules.default
                ];
              };
            })
          ];
      });
}
