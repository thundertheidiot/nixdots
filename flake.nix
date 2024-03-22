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

    hyprland.url = "github:hyprwm/Hyprland";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };
    # split-monitor-workspaces = {
    #   url = "github:Duckonaut/split-monitor-workspaces";
    #   inputs.hyprland.follows = "hyprland";
    # };


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
          # inputs.base16.homeManagerModule
          {
            programs.home-manager.enable = true;
            targets.genericLinux.enable = true;
            # scheme = "${inputs.tt-schemes}/base16/tokyo-night-dark.yaml";
          }
          # ./home
          (import ./home { inherit pkgs localconfig inputs; })
        ];
      };

		  nixosModules.default = { config, pkgs, lib, ... }: {
			  imports = [
          localconfig.systemConfig
          (import ./base { inherit config pkgs localconfig; })
			  ];
        
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

            # home-manager.users.${localconfig.username} = import ./home;
            # TODO: fix this shit, scheme needs to be passed in
            home-manager.users.${localconfig.username} = import ./home { inherit pkgs localconfig inputs; };
          }
			  ];
		  };
	  };
}
