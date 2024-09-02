{
  description = "nixos";

  nixConfig = {
    substituters = [
      "https://hyprland.cachix.org"
      "https://cosmic.cachix.org/"
      "https://nyx.chaotic.cx/"
      # "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="
      "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8="
      # "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  outputs = {
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    mlib = (import ./lib) {lib = nixpkgs.lib;};
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
      ({config, ...}: {
        age.identityPaths = ["${config.homeDirectory}/.ssh/id_agenix"];
        age.secrets.kodi_youtube_api_keys.file = ./secrets/kodi_youtube_api_keys.age;
      })
    ];

    nixosModules.default = {...}: {
      imports = ./modules;
    };

    mkSystem = cfg: extramodules: let
      common = commonModules cfg;
    in
      nixpkgs.lib.nixosSystem {
        system = cfg.systemArch;
        pkgs = let
          p = (import nixpkgs {system = cfg.systemArch;}).applyPatches;
        in
          import (p {
            name = "nixpkgs-patched";
            src = "${nixpkgs}";
            patches = import ./patches;
          }) {system = cfg.systemArch;};
        specialArgs = {inherit inputs mlib mpkgs;};
        modules =
          common
          ++ extramodules
          ++ [
            cfg.system
            home-manager.nixosModules.home-manager
            inputs.lix-module.nixosModules.default # lix
            inputs.cosmic.nixosModules.default
            inputs.hyprland.nixosModules.default
            inputs.agenix.nixosModules.default
            inputs.chaotic.nixosModules.default
            inputs.disko.nixosModules.default
            ({config, ...}: {
              time.timeZone = config.timeZone;
              networking.hostName = config.hostName;

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
                  inputs.waybar.overlays.default
                  (final: prev: rec {
                    nur = import inputs.nur {
                      nurpkgs = prev;
                      pkgs = prev;
                    };
                    firefox-addons = nur.repos.rycee.firefox-addons;
                    ataraxiasjel = nur.repos.ataraxiasjel;
                    "2405" = import inputs.nixpkgs-24-05 {
                      system = final.system;
                      config.allowUnfree = final.config.allowUnfree;
                    };
                    "2311" = import inputs.nixpkgs-23-11 {
                      system = final.system;
                      config.allowUnfree = final.config.allowUnfree;
                    };
                    agenix = inputs.agenix.packages.${final.system};
                    awesome = inputs.nixpkgs-f2k.packages.${final.system}.awesome-git;

                    # TODO: remove when works
                    avrdude = prev.avrdude.overrideAttrs (prev: {
                      src = prev.src.override {
                        repo = "avrdude";
                      };
                    });

                    vulkan-validation-layers = prev.vulkan-validation-layers.overrideAttrs (oldAttrs: {
                      buildInputs = oldAttrs.buildInputs ++ [prev.spirv-tools];
                    });

                    libzip = prev.libzip.overrideAttrs (old: {
                      patches = [
                        # https://github.com/nih-at/libzip/issues/404
                        (final.fetchpatch2 {
                          name = "Check-for-zstd_TARGET-before-using-it-in-a-regex.patch";
                          url = "https://github.com/nih-at/libzip/commit/c719428916b4d19e838f873b1a177b126a080d61.patch";
                          hash = "sha256-4yjosuvVN/kPmmBtxxVXjOWrI3hdKJPQZrqL6BztZo8=";
                        })
                      ];
                    });

                    hyprland-split-monitor-workspaces = inputs.split-monitor-workspaces.packages.${final.system}.split-monitor-workspaces;
                  })
                ];
              };

              imports =
                (import ./modules)
                ++ [
                  ./modules/nixos
                ];

              meow.home = {
                user = config.username;
                extraSpecialArgs = {inherit inputs mlib mpkgs;};
                sharedModules =
                  common
                  ++ [
                    # cfg.home
                    inputs.agenix.homeManagerModules.default
                    inputs.plasma-manager.homeManagerModules.plasma-manager
                    inputs.base16.homeManagerModule
                  ];
                modules = [
                  ./modules/home
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
    # https://hydra.nixos.org/job/nixpkgs/trunk/unstable
    # https://status.nixos.org/
    # nixpkgs.url = "github:NixOS/nixpkgs?ref=8a5184b51f449368db552406a762eccd5079a959"; # freetube unstable
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-24-05.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-23-11.url = "github:NixOS/nixpkgs/nixos-23.11";

    naersk.url = "github:nix-community/naersk";
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.90.0-rc1.tar.gz";
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

    base16.url = "github:SenchoPens/base16.nix";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-f2k.url = "github:moni-dz/nixpkgs-f2k";

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

    agenix.url = "github:ryantm/agenix";
  };
}
