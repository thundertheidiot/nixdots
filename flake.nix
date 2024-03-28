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
      url = "github:hyprwm/Hyprland";
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

  outputs = {
    self,
    nixpkgs,
    home-manager,
    chaotic,
    ...
  } @ inputs: let
    localconfig = import ./local.nix;
  in {
    defaultPackage.${localconfig.system} = home-manager.defaultPackage.${localconfig.system};

    common.nixpkgs = {inputs, ...}: {
      nixpkgs = {
        system = localconfig.system;
        config.allowUnfree = true;
        overlays = [
          inputs.emacs-overlay.overlay
          inputs.nixgl.overlay
          (final: prev: {
            nur = import inputs.nur {
              nurpkgs = prev;
              pkgs = prev;
            };
            "2311" = import inputs.nixpkgs-23-11 {
              system = final.system;
              config.allowUnfree = final.config.allowUnfree;
            };
            agenix = inputs.agenix.packages.${localconfig.system};
          })
        ];
      };
    };

    common.agenix = {
      age.identityPaths = [ "${localconfig.homeDirectory}/.ssh/id_agenix" ];

      age.secrets.youtube_api_keys.file = ./secrets/youtube_api_keys.age;
    };

    homeConfigurations.${localconfig.username} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {system = localconfig.system;};
      extraSpecialArgs = {
        inherit localconfig inputs;
      };
      modules = [
        self.common.nixpkgs
        inputs.agenix.homeManagerModules.default
        self.common.agenix
        # chaotic.homeManagerModules.default
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
      system = localconfig.system;
      specialArgs = {
        inherit localconfig inputs;
      };
      modules = [
        self.common.nixpkgs
        inputs.agenix.nixosModules.default
        self.common.agenix
        {
          chaotic.nyx.cache.enable = true;
          time.timeZone = localconfig.timeZone;
          networking.hostName = localconfig.hostName;

          imports = [
            localconfig.systemConfig
            ./nixos
          ];
        }
        chaotic.nixosModules.default
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {
            inherit localconfig inputs;
          };

          nixpkgs.config.allowUnfree = true;

          home-manager.users.${localconfig.username} = ./home;
        }
      ];
    };
  };
}
