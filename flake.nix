{
  description = "nixos";

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

    nix-autobahn.url = "github:Lassulus/nix-autobahn";
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    chaotic,
    ...
  } @ inputs: let
    localconfig = import ./local.nix;
  in {
    defaultPackage.${localconfig.systemArch} = home-manager.defaultPackage.${localconfig.systemArch};

    common.nixpkgs = {inputs, localconfig, ...}: {
      nixpkgs = {
        system = localconfig.systemArch;
        config.allowUnfree = true;
        overlays = [
          inputs.emacs-overlay.overlay
          inputs.nixgl.overlay
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
    };

    common.agenix = {
      age.identityPaths = [ "${localconfig.homeDirectory}/.ssh/id_agenix" ];

      age.secrets.kodi_youtube_api_keys.file = ./secrets/kodi_youtube_api_keys.age;
      # age.secrets.kodi_jellyfin_data.file = ./secrets/kodi_jellyfin_data.age;
    };

    homeConfigurations.${localconfig.username} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {system = localconfig.systemArch;};
      extraSpecialArgs = {
        inherit localconfig inputs;
      };
      modules = [
        ./options.nix
        localconfig.options
        self.common.nixpkgs
        inputs.agenix.homeManagerModules.default
        self.common.agenix
        localconfig.home
        ({
          config,
          pkgs,
          ...
        }:
          with config; {
            home.packages = with pkgs; [
              nixgl.nixGLIntel # deceptive naming, this is for mesa drivers
              nixgl.nixVulkanIntel
            ];
            programs.home-manager.enable = true;
            targets.genericLinux.enable = true;
          })
        ./home
      ];
    };

    nixosConfigurations.default = nixpkgs.lib.nixosSystem {
      system = localconfig.systemArch;
      specialArgs = {
        inherit localconfig inputs;
      };
      modules = [
        ./options.nix
        self.common.nixpkgs
        localconfig.options
        inputs.agenix.nixosModules.default
        self.common.agenix
        ({config, ...}: {
          time.timeZone = config.timeZone;
          networking.hostName = config.hostName;

          imports = [
            localconfig.system
            ./nixos
          ];
        })
        chaotic.nixosModules.default
        home-manager.nixosModules.home-manager
        ({ config, ... }: {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {
            inherit localconfig inputs;
          };

          home-manager.sharedModules = [
            inputs.agenix.homeManagerModules.default
            self.common.agenix
            ./options.nix
            localconfig.options
            localconfig.home
          ];

          home-manager.users.${config.username} = ./home;
        })
      ];
    };
  };
}
