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
      example = "/any/other/place/for/a/home/directory";
      description = "User home directory.";
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

    monitors = lib.mkOption {
      default = [];
      type = lib.types.listOf lib.types.attrs;
      example = [
        {
          name = "DP-3";
          width = 2560;
          height = 1440;
          refresh = 144;
          x = 1920;
        }
        {
          name = "DP-1";
          width = 1920;
          height = 1080;
          refresh = 144;
        }
      ];
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
      type = lib.types.enum ["hyprland" "plasma"]; # TODO: awesomewm, swayfx, qtile maybe
      default = "hyprland";
      example = "plasma";
      description = "What \"desktop environment\" to install and configure.";
    };

    workstation.plasma.tilingwm = lib.mkEnableOption "Configure plasma into a tiling wm environment.";

    setup.userMachine.enable = lib.mkEnableOption (
      lib.mdDoc "Base setup for a non server machine, for example a login manager, gui programs, gtk theme, etc."
    );

    setup.hyprland.enable = lib.mkEnableOption (
      lib.mdDoc "Hyprland windowmanager."
    );

    setup.swayfx.enable = lib.mkEnableOption (
      lib.mdDoc "Swayfx windowmanager."
    );

    setup.hyprland.extraConfig = lib.mkOption {
      default = "";
      example = "monitor=DP-3, 2560x1440@144, 0x0, 1";
      description = "Extra configuration for hyprland.";
    };

    setup.hyprland.extraAutostart = lib.mkOption {
      default = [];
      type = lib.types.listOf lib.types.nonEmptyStr;
      example = ["gajim"];
      description = "Extra exec-once lines for hyprland.";
    };

    setup.awesomeWM.enable = lib.mkEnableOption (
      lib.mdDoc "AwesomeWM windowmanager."
    );

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

    setup.phone = lib.mkEnableOption (lib.mdDoc "Phone setup");

    setup.gpu = lib.mkOption {
      type = lib.types.enum ["amd" "nvidia" "intel" "none"];
      default = "none";
      example = "amd";
      description = "Gpu drivers to install.";
    };
  };
}
