{
  description = "nixos";

  nixConfig = {
    substituters = [
      "https://hyprland.cachix.org"
      "https://cosmic.cachix.org/"
      "https://nyx.chaotic.cx/"
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="
      "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  outputs = {
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
    mlib = (import ./lib) {inherit lib;};
    mpkgs = import ./pkgs;
  in rec {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;

    nixosConfigurations =
      gen ["desktop" "x220" "t440p" "digiboksi"]
      // {
        local = mkSystem (import ./local.nix) [];
        iso = mkSystem (import ./hosts/iso.nix) [
          "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares.nix"
        ];
      };

    commonModules = cfg: [
      ./options.nix
      cfg.options
    ];

    nixosModules.default = {...}: {
      imports = ./modules;
    };

    mkSystem = cfg: extramodules: let
      common = commonModules cfg;
    in
      lib.nixosSystem {
        system = cfg.systemArch;
        specialArgs = {inherit inputs mlib mpkgs;};
        modules =
          common
          ++ extramodules
          ++ [
            cfg.system
            home-manager.nixosModules.home-manager
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
                  # "https://nix-community.cachix.org"
                ];
                trusted-public-keys = [
                  "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
                  "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="
                  # "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
                ];
              };

              nixpkgs = {
                system = config.systemArch;
                config.allowUnfree = true;
                overlays = [
                  inputs.emacs-overlay.overlay
                  # inputs.hyprland.overlays.default
                  inputs.waybar.overlays.default
                  (import ./overrides.nix {inherit inputs;})
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

    naersk.url = "github:nix-community/naersk";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.0.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cosmic = {
      url = "github:lilyinstarlight/nixos-cosmic";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix.url = "github:danth/stylix";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    hyprland = {
      # latest before clipboard break
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=4cdddcfe466cb21db81af0ac39e51cc15f574da9";
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=9a09eac79b85c846e3a865a9078a3f8ff65a9259";
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    };

    waybar.url = "github:Alexays/Waybar";

    split-monitor-workspaces = {
      # for the ref above
      # url = "github:thundertheidiot/split-monitor-workspaces?ref=d6efa8bd7254f40a2009d406934059fda539811d";
      url = "github:thundertheidiot/split-monitor-workspaces";
      inputs.hyprland.follows = "hyprland";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nix-gaming.url = "github:fufexan/nix-gaming";

    nur.url = "github:nix-community/NUR";

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    sops-nix.url = "github:Mic92/sops-nix";
  };
}
