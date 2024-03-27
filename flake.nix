{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

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

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    sops-nix,
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
          })
        ];
      };
    };

    common.sops = {
      sops = {
        # defaultSopsFile = ./secrets/secrets.yaml;
        # defaultSopsFormat = "yaml";
        
        age.keyFile = "${localconfig.homeDirectory}/.config/sops/age/keys.txt";
        # secrets."youtube/api_key"= {};
        # secrets."youtube/client_id" = {};
        # secrets."youtube/client_secret" = {};
      };
    };

    homeConfigurations.${localconfig.username} = home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs {system = localconfig.system;};
      extraSpecialArgs = {
        inherit localconfig inputs;
      };
      modules = [
        self.common.nixpkgs
        sops-nix.homeManagerModules.sops
        self.common.sops
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
        sops-nix.nixosModules.sops
        self.common.sops
        {
          time.timeZone = localconfig.timeZone;
          networking.hostName = localconfig.hostName;

          imports = [
            localconfig.systemConfig
            ./nixos
          ];
        }
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
