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

    # emacs packages
    emacs-eglot-booster = {
      url = "github:jdtsmith/eglot-booster";
      flake = false;
    };

    nixpkgs-f2k.url = "github:moni-dz/nixpkgs-f2k";

    hyprland.url = "github:hyprwm/Hyprland";
    # hyprland-plugins = {
    #   url = "github:hyprwm/hyprland-plugins";
    #   inputs.hyprland.follows = "hyprland";
    # };

    split-monitor-workspaces = {
      url = "github:Dekomoro/split-monitor-workspaces";
      inputs.hyprland.follows = "hyprland";
    };

	};

	outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      localconfig = import ./local.nix;
      pkgs = import nixpkgs { system = localconfig.system; config.allowUnfree = true; };
    in {
      defaultPackage.${localconfig.system} = home-manager.defaultPackage.${localconfig.system};

      homeConfigurations.${localconfig.username} = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        extraSpecialArgs = {
          inherit localconfig inputs;
        };
        modules = [
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
          {
            time.timeZone = localconfig.timeZone;
            networking.hostName = localconfig.hostName;

            imports = [
              localconfig.systemConfig
              ./nixos
            ];
          }
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit localconfig inputs;
            };

            nixpkgs.config.allowUnfree = true;

            home-manager.users.${localconfig.username} = ./home;
            # home-manager.users.${localconfig.username} = import ./home {
            #   inherit pkgs localconfig inputs;
            # };
          }
			  ];
		  };
	  };
}
