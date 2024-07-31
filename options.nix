{
  lib,
  config,
  ...
}: {
  options = {
    username = lib.mkOption {
      default = "thunder";
      type = lib.types.nonEmptyStr;
      example = "sugondese";
      description = "User account username.";
    };

    homeDirectory = lib.mkOption {
      default = "/home/${config.username}";
      type = lib.types.nonEmptyStr;
      example = "/any/other/place/for/a/directory";
      description = "User home directory.";
    };

    stubbornHomeDirectory = lib.mkOption {
      default = "${config.homeDirectory}/.local/state/home";
      type = lib.types.nonEmptyStr;
      example = "/any/other/place/for/a/directory";
      description = "Fake home directory for stubborn applications.";
    };

    hostName = lib.mkOption {
      default = "nixos";
      example = "idk";
      type = lib.types.nonEmptyStr;
      description = "System hostname.";
    };

    systemArch = lib.mkOption {
      default = "x86_64-linux";
      type = lib.types.nonEmptyStr;
      example = "i686-linux";
      description = "System architechture, used for nixpkgs.";
    };

    timeZone = lib.mkOption {
      default = "Europe/Helsinki";
      example = "Europe/London";
      description = "Timezone.";
    };

    workstation.enable = lib.mkEnableOption (
      lib.mdDoc "User facing machine, e.g. a laptop or a desktop computer."
    );

    tv.enable = lib.mkEnableOption (
      lib.mdDoc "Configures kodi as a tv frontend thingy"
    );

    workstation.keyd.enable = lib.mkEnableOption "Keyboard settings through keyd.";
    workstation.keyd.ids = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = ["*"];
      example = ["1ea7:0907"];
      description = "Device ids for keyd";
    };

    workstation.laptop = lib.mkEnableOption "Power management.";

    workstation.utils = lib.mkOption {
      type = lib.types.enum ["generic/gtk" "kde"];
      default = "generic/gtk";
      example = "kde";
      description = "The \"family of programs\" to use for utilities like ssh askpass or polkit.";
    };

    workstation.environment = lib.mkOption {
      type = lib.types.listOf (lib.types.enum ["hyprland" "plasma" "cosmic"]);
      default = ["hyprland"];
      example = ["plasma"];
      description = "What \"desktop environment\" to install and configure.";
    };

    workstation.plasma.tilingwm = lib.mkEnableOption "Configure plasma into a tiling wm environment.";

    emacs.enable = lib.mkEnableOption "Enable emacs";
    emacs.flavor = lib.mkOption {
      type = lib.types.listOf (lib.types.enum ["doom" "custom"]);
      default = ["doom"];
      description = "\"Flavor\" of emacs to install, will not exist forever."; # TODO: remove custom
    };

    setup.hyprland.extraConfig = lib.mkOption {
      default = "";
      example = "monitor=DP-3, 2560x1440@144, 0x0, 1";
      description = "Extra configuration for hyprland.";
    };

    setup.hyprland.forceMultiMonitor = lib.mkEnableOption "Enable multi monitor workspaces plugin, even if multiple monitors aren't present.";

    setup.hyprland.extraAutostart = lib.mkOption {
      default = [];
      type = lib.types.listOf lib.types.nonEmptyStr;
      example = ["gajim"];
      description = "Extra exec-once lines for hyprland.";
    };

    setup.firefox.enable = lib.mkEnableOption (
      lib.mdDoc "Nix-managed firefox."
    );

    setup.gaming.enable = lib.mkEnableOption (
      lib.mdDoc "Install gaming-related programs and settings."
    );

    setup.tv.enable = lib.mkEnableOption (
      lib.mdDoc "Kodi and other stuff to make this machine into a smart tv."
    );

    setup.laptop.enable = lib.mkEnableOption (
      lib.mdDoc "Laptop specific things"
    );

    setup.desktop.enable = lib.mkEnableOption (
      lib.mdDoc "Desktop specific things"
    );

    setup.gpu = lib.mkOption {
      type = lib.types.enum ["amd" "nvidia" "intel" "none"];
      default = "none";
      example = "amd";
      description = "Gpu drivers to install.";
    };
  };
}
