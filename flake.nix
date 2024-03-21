{
	description = "nixos";

	inputs = {
		nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
		home-manager = {
			url = "github:nix-community/home-manager";
			inputs.nixpkgs.follows = "nixpkgs";
		};
    # emacs-overlay = {
    #   url = "github:nix-community/emacs-overlay";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    # emacs packages
    emacs-eglot-booster = {
      url = "github:jdtsmith/eglot-booster";
      flake = false;
    };

    hyprland.url = "github:hyprwm/Hyprland";
    split-monitor-workspaces = {
      url = "github:Duckonaut/split-monitor-workspaces";
      inputs.hyprland.follows = "hyprland";
    };
	};

	outputs = { self, nixpkgs, home-manager, ... }@inputs:
    let
      localconfig = import ./local.nix;
      pkgs = import nixpkgs { system = localconfig.system; };
    in {
      defaultPackage.${localconfig.system} = home-manager.defaultPackage.${localconfig.system};

      homeConfigurations.${localconfig.username} = home-manager.lib.homeManagerConfiguration {
        pkgs = pkgs;
        modules = [
          {
            programs.home-manager.enable = true;
            targets.genericLinux.enable = true;
          }
          (if localconfig.install.hyprland then inputs.hyprland.homeManagerModules.default else {})
          # localconfig.homeManagerConfig
          (import ./home { inherit pkgs localconfig inputs; })
          # ./home
        ];
      };

		  nixosModules.default = { config, pkgs, lib, ... }: {
			  imports = [
          localconfig.systemConfig
          (import ./base { inherit config pkgs localconfig; })
			  ];

        users.users.${localconfig.username}.isNormalUser = true;
        
			  time.timeZone = localconfig.timeZone;
        networking.hostName = localconfig.hostName;
		  };

		  nixosConfigurations.default = nixpkgs.lib.nixosSystem {
			  system = "x86_64-linux";
			  modules = [
				  self.nixosModules.default
          home-manager.nixosModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;

            home-manager.users.${localconfig.username} = import ./home { inherit pkgs localconfig inputs; };
          }
			  ];
		  };
	  };
}
