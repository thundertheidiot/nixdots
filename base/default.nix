{ config, pkgs, localconfig, ... }: {
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.allowed-users = [ localconfig.username ];

	i18n.defaultLocale = "en_US.UTF-8";
	console = {
		useXkbConfig = true;
	};
	
	networking.networkmanager.enable = true;

	services.xserver.enable = true;
	services.xserver.displayManager.gdm.enable = true;
	services.xserver.desktopManager.gnome.enable = true;

	services.xserver = {
		xkb.layout = "us";
		xkb.options = "eurosign:e,caps:escape";
		libinput.enable = true;
	};

	sound.enable = true;

	# users.users.${localconfig.username} = {
	# 	extraGroups = [ "wheel" ];
	# 	packages = with pkgs; [
	# 		firefox
	# 		tree
	# 		alacritty
	# 	];
	# };

	environment.systemPackages = with pkgs; [
		neovim
		wget
		git
	];

	services.openssh.enable = true;
}
