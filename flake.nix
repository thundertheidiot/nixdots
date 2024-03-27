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
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    localconfig = import ./local.nix;
    # pkgs = import nixpkgs {
    #   system = localconfig.system;
    #   config.allowUnfree = true;
    #   overlays = [
    #     inputs.emacs-overlay.overlay
    #     inputs.nixgl.overlay
    #     (final: prev: {
    #       nur = import inputs.nur {
    #         nurpkgs = prev;
    #         pkgs = prev;
    #       };
    #     })
    #   ];
    # };
  in {
    defaultPackage.${localconfig.system} = home-manager.defaultPackage.${localconfig.system};

    nixglModule = {
      config,
      pkgs,
      ...
    }:
      with config; {
        home.packages = with pkgs; [
          nixgl.nixGLIntel
        ];
      };

    commonModules.nixpkgs = { inputs, ... }: {
      nixpkgs.system = localconfig.system;
      nixpkgs.config.allowUnfree = true;
      nixpkgs.overlays = [
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

    homeConfigurations.${localconfig.username} = home-manager.lib.homeManagerConfiguration {
      # inherit pkgs;
      pkgs = import nixpkgs { system = localconfig.system; };
      extraSpecialArgs = {
        inherit localconfig inputs;
      };
      modules = [
        self.commonModules.nixpkgs
        self.nixglModule
        {
          programs.home-manager.enable = true;
          targets.genericLinux.enable = true;
        }
        ./home
      ];
    };

    nixosConfigurations.default = nixpkgs.lib.nixosSystem {
      system = localconfig.system;
      specialArgs = {
        inherit localconfig inputs;
      };
      modules = [
        self.commonModules.nixpkgs
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
