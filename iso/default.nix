# TODO make the iso
{
  lib,
  inputs,
  ...
}: let
  mlib = (import ../lib) {inherit lib;};
in
  lib.nixosSystem rec {
    system = "x86_64-linux";
    specialArgs = {inherit inputs mlib;};

    modules = [
      "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-base.nix"
      "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
      inputs.home-manager.nixosModules.home-manager
      inputs.chaotic.nixosModules.default
      inputs.sops-nix.nixosModules.default
      inputs.disko.nixosModules.default
      inputs.lix-module.nixosModules.default
      inputs.cosmic.nixosModules.default
      inputs.hyprland.nixosModules.default
      inputs.stylix.nixosModules.stylix
      ../sops
      {
        imports = import ../modules;

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
          inherit system;
          config.allowUnfree = true;
          overlays = [
            inputs.emacs-overlay.overlay
            # inputs.hyprland.overlays.default
            inputs.waybar.overlays.default
            inputs.rust-overlay.overlays.default
            (import ../overrides.nix {inherit inputs;})
          ];
        };

        networking.wireless.enable = lib.mkForce false;
        networking.hostName = "iso";
        time.timeZone = "Europe/Helsinki";

        system.stateVersion = "24.11";

        meow = {
          workstation.enable = true;
          workstation.environment = ["gnome"];
          workstation.flatpak.enable = false;

          shell.enable = true;
        };

        services.displayManager.sddm = {
          settings = {
            Autologin = {
              Session = "gnome.desktop";
              User = "nixos";
            };
          };
        };

        meow.home = {
          user = "nixos";
          extraSpecialArgs = {inherit inputs mlib;};
          stateVersion = "24.11";
          sharedModules = [
            inputs.plasma-manager.homeManagerModules.plasma-manager
          ];
        };
      }
    ];
  }
