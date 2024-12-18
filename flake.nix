{
  description = "nixos";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://hyprland.cachix.org"
      "https://cosmic.cachix.org/"
      "https://chaotic-nyx.cachix.org/"
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="
      "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  outputs = {self, ...} @ inputs: let
    # hybrid flake + npins
    # npins = import ./npins/wrapper.nix;
    # inputs = inputs' // npins;
    lib = inputs.nixpkgs.lib;
    mlib = (import ./lib) {inherit lib;};
    mpkgs = import ./pkgs;
  in rec {
    formatter.x86_64-linux = inputs.nixpkgs.legacyPackages.x86_64-linux.alejandra;

    deploy.nodes = {
      server = {
        hostname = "192.168.101.101";
        profiles.system = {
          user = "root";
          sshUser = "root";
          path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.server;
        };
      };
    };

    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;

    nixosConfigurations =
      gen ["desktop" "server" "x220" "t440p" "digiboksi"]
      // {
        # iso = (import ./iso) {inherit lib inputs;};
        # iso = mkSystem (import ./hosts/iso.nix) [
        #   "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares.nix"
        # ];
      };

    commonModules = cfg: [
      ./options.nix
      (cfg.options or {})
    ];

    nixosModules.default = {...}: {
      imports = ./modules;
    };

    mkSystem = cfg: extramodules: let
      common = commonModules cfg;
    in
      lib.nixosSystem {
        specialArgs = {inherit inputs mlib mpkgs;};
        modules =
          common
          ++ extramodules
          ++ [
            cfg.system
            inputs.home-manager.nixosModules.home-manager
            inputs.chaotic.nixosModules.default
            inputs.sops-nix.nixosModules.default
            inputs.disko.nixosModules.default
            inputs.lix-module.nixosModules.default
            inputs.cosmic.nixosModules.default
            inputs.hyprland.nixosModules.default
            inputs.stylix.nixosModules.stylix
            ./sops
            ({config, ...}: {
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
                config.allowUnfree = true;
                overlays = [
                  inputs.emacs-overlay.overlay
                  # inputs.hyprland.overlays.default
                  inputs.waybar.overlays.default
                  inputs.rust-overlay.overlays.default
                  inputs.nixpkgs-xr.overlays.default
                  (import ./overrides.nix {inherit lib inputs;})
                ];
              };

              imports = import ./modules;

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
      };

    gen = hosts:
      builtins.listToAttrs (builtins.map
        (s: {
          name = s;
          value = mkSystem (mlib.getHostConfig s) [];
        })
        hosts);
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-24-05.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-24-11.url = "github:NixOS/nixpkgs/nixos-24.11";

    naersk.url = "github:nix-community/naersk";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.1-2.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    cosmic = {
      url = "github:lilyinstarlight/nixos-cosmic";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix.url = "github:danth/stylix";
    stylix.inputs.home-manager.follows = "home-manager";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-xr.url = "github:nix-community/nixpkgs-xr";

    rust-overlay.url = "github:oxalica/rust-overlay";

    screenshot.url = "./pkgs/screenshot";

    hyprland = {
      # latest before clipboard break
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=4cdddcfe466cb21db81af0ac39e51cc15f574da9";
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=9a09eac79b85c846e3a865a9078a3f8ff65a9259";
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    };

    waybar.url = "github:Alexays/Waybar";

    hyprsplit = {
      url = "github:shezdy/hyprsplit";
      inputs.hyprland.follows = "hyprland";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nix-gaming.url = "github:fufexan/nix-gaming";

    nur.url = "github:nix-community/NUR";

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    sops-nix.url = "github:Mic92/sops-nix";
  };
}
