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

  mkSystem = {
    nixosSystem,
    inputs,
    mlib,
    mpkgs,
    config,
    extraModules ? [],
  }:
    nixosSystem (let
      common = [
        ../options.nix
        (config.options or {})
      ];
    in {
      system = builtins.currentSystem;
      specialArgs = {inherit inputs mlib mpkgs;};
      modules =
        extraModules
        ++ common
        ++ [
          config.system
          ../sops

          inputs.home-manager.nixosModules.home-manager
          inputs.chaotic.nixosModules.default
          inputs.sops-nix.nixosModules.default
          inputs.disko.nixosModules.default
          inputs.lix-module.nixosModules.default
          inputs.cosmic.nixosModules.default
          inputs.hyprland.nixosModules.default
          inputs.stylix.nixosModules.stylix

          ({
            lib,
            config,
            ...
          }: {
            nix.settings = {
              substituters = [
                "https://hyprland.cachix.org"
                "https://cosmic.cachix.org/"
                "https://cache.iog.io" # haskell.nix
                "https://nix-community.cachix.org"
              ];
              trusted-public-keys = [
                "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
                "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="
                "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # haskell.nix
                "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
              ];
            };

            nixpkgs = {
              system = config.systemArch;
              config.allowUnfree = true;
              overlays = [
                inputs.emacs-overlay.overlay
                # inputs.hyprland.overlays.default
                inputs.waybar.overlays.default
                inputs.rust-overlay.overlays.default
                inputs.nixpkgs-xr.overlays.default
                (import ../overrides.nix {inherit lib inputs;})
              ];
            };

            imports = import ../modules;

            meow.home = {
              user = config.username;
              extraSpecialArgs = {inherit inputs mlib mpkgs;};
              sharedModules =
                common
                ++ [
                  inputs.plasma-manager.homeManagerModules.plasma-manager
                ];
            };
          })
        ];
    });

  homeModule = module: {
    meow.home.modules = [module];
  };
}
