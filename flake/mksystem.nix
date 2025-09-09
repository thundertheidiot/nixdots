{
  lib,
  config,
  inputs,
  ...
}
: let
  inherit (lib) isAttrs isFunction isList evalModules mkOption;
  inherit (lib.types) listOf str;

  root = inputs.self.outPath;
in {
  flake.mkSystem = {modules}:
    assert isList modules;
      lib.nixosSystem (let
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
            ({...}: {
              imports = import "${root}/modules";
              nixpkgs = {
                overlays = [
                  inputs.emacs-overlay.overlay
                  inputs.rust-overlay.overlays.default
                  inputs.nixpkgs-xr.overlays.default
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
