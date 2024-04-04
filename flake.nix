{
  description = "nixos";

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
  in rec {
    nixosConfigurations = gen ["desktop" "x220"] // {local = mkSystem (import ./local.nix);};

    mkSystem = cfg:
      lib.nixosSystem {
        system = cfg.systemArch;
        specialArgs = { inherit inputs; };
        modules = [
          ./options.nix
          cfg.options
          cfg.system
          home-manager.nixosModules.home-manager
          inputs.agenix.nixosModules.default
          ({config, ...}: {
            age.identityPaths = ["${config.homeDirectory}/.ssh/id_agenix"];
            age.secrets.kodi_youtube_api_keys.file = ./secrets/kodi_youtube_api_keys.age;

            time.timeZone = config.timeZone;
            networking.hostName = config.hostName;

            nixpkgs = {
              system = config.systemArch;
              config.allowUnfree = true;
              overlays = [
                inputs.emacs-overlay.overlay
                (final: prev: rec {
                  nur = import inputs.nur {
                    nurpkgs = prev;
                    pkgs = prev;
                  };
                  firefox-addons = nur.repos.rycee.firefox-addons;
                  "2311" = import inputs.nixpkgs-23-11 {
                    system = final.system;
                    config.allowUnfree = final.config.allowUnfree;
                  };
                  agenix = inputs.agenix.packages.${final.system};
                  hyprland = inputs.hyprland.packages.${final.system}.hyprland;
                  awesome = inputs.nixpkgs-f2k.packages.${final.system}.awesome-git;
                })
              ];
            };

            imports = [
              ./nixos
            ];

            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = {inherit inputs;};

              sharedModules = [
                ./options.nix
                cfg.home
                cfg.options
                inputs.agenix.homeManagerModules.default
              ];

              users.${config.username} = ./home;
            };
          })
        ];
      };

    gen = hosts:
      builtins.listToAttrs (builtins.map (s: {
        name = s;
        value = mkSystem (import ./hosts/${s}.nix);
      })
      hosts);
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-23-11.url = "github:NixOS/nixpkgs/nixos-23.11";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    base16.url = "github:SenchoPens/base16.nix";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-f2k.url = "github:moni-dz/nixpkgs-f2k";

    hyprland = {
      url = "github:hyprwm/Hyprland?ref=v0.37.1";
    };

    split-monitor-workspaces = {
      url = "github:Dekomoro/split-monitor-workspaces";
      inputs.hyprland.follows = "hyprland";
    };

    # Deliberately doesn't follow nixpkgs, i want to be able to rollback emacs
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixgl.url = "github:nix-community/nixGL";
    nix-gaming.url = "github:fufexan/nix-gaming";

    nur.url = "github:nix-community/NUR";

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    agenix.url = "github:ryantm/agenix";
  };
}
