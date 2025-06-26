{...}: let
  inherit (builtins) readFileType pathExists;
in {
  getHostConfig = host: let
    file = ../hosts/${host}.nix;
    dir = ../hosts/${host};
  in
    if (pathExists dir) && (readFileType dir) == "directory"
    then import dir
    else if (pathExists file) && (readFileType file) == "regular"
    then import file
    else throw "No ${file} or ${dir}/default.nix";

  homeModule = module: {
    meow.home.modules = [module];
  };

  mkSystem = {
    nixosSystem,
    inputs,
    mlib,
    config,
    extraModules ? [],
  }:
    nixosSystem {
      specialArgs = {inherit inputs mlib;};
      modules =
        extraModules
        ++ [
          config
          ../sops

          inputs.home-manager.nixosModules.home-manager
          inputs.chaotic.nixosModules.default
          inputs.sops-nix.nixosModules.default
          inputs.disko.nixosModules.default
          inputs.hyprland.nixosModules.default
          inputs.stylix.nixosModules.stylix
          inputs.authentik-nix.nixosModules.default

          inputs.nix-gaming.nixosModules.pipewireLowLatency

          ({
            lib,
            config,
            ...
          }: {
            imports = import ../_legacy_modules;

            nixpkgs = {
              overlays = [
                inputs.emacs-overlay.overlay
                # inputs.waybar.overlays.default
                inputs.rust-overlay.overlays.default
                # inputs.hyprland.overlays.default
                inputs.nixpkgs-xr.overlays.default
                (import ../overrides.nix {inherit lib inputs;})
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
    };
}
