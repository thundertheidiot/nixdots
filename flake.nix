{
  description = "nixos";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
    unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "unstable";
    };

    base16.url = "github:SenchoPens/base16.nix";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-f2k.url = "github:moni-dz/nixpkgs-f2k";

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "unstable";
    };

    split-monitor-workspaces = {
      url = "github:Dekomoro/split-monitor-workspaces";
      inputs.hyprland.follows = "hyprland";
    };

    # Deliberately doesn't follow nixpkgs, i want to be able to rollback emacs
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixgl.url = "github:nix-community/nixGL";
    nix-gaming = {
      url = "github:fufexan/nix-gaming";
      inputs.nixpkgs.follows = "unstable";
    }; 
  };

  outputs = {
    self,
    nixpkgs,
    unstable,
    home-manager,
    ...
  } @ inputs: let
    localconfig = import ./local.nix;
    pkgs = import nixpkgs {
      system = localconfig.system;
      config.allowUnfree = true;
      overlays = [
        inputs.emacs-overlay.overlay
      ];
    };
    unstablePkgs = import unstable {
      system = localconfig.system;
      config.allowUnfree = true;
      overlays = [
        inputs.emacs-overlay.overlay
        inputs.nixgl.overlay
      ];
    };
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

    homeConfigurations.${localconfig.username} = home-manager.lib.homeManagerConfiguration {
      # pkgs = import nixpkgs {
      #   system = localconfig.system;
      #   config.allowUnfree = true;
      #   overlays = [inputs.nixgl.overlay inputs.emacs-overlay.overlay];
      # };
      pkgs = unstablePkgs;
      extraSpecialArgs = {
        inherit localconfig inputs;
      };
      modules = [
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
        inherit unstablePkgs localconfig inputs;
      };
      modules = [
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
          pkgs = unstablePkgs;
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {
            inherit localconfig inputs;
          };

          home-manager.users.${localconfig.username} = ./home;
        }
      ];
    };
  };
}
