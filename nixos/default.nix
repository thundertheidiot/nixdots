{
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}: {
  nix.settings.experimental-features = ["nix-command" "flakes"];
  nix.settings.allowed-users = [localconfig.username];

  security.sudo.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    useXkbConfig = true;
  };

  networking.networkmanager.enable = true;

  services.xserver.enable = true;
  services.xserver.displayManager.lightdm = {
    enable = true;
  };

  services.xserver = {
    xkb.layout = "us";
    xkb.options = "eurosign:e";
    libinput.enable = true;
  };

  sound.enable = true;

  users.users.${localconfig.username} = {
    extraGroups = ["wheel"];
    isNormalUser = true;
  };

  environment.systemPackages = with pkgs;
    [
      neovim
      wget
      git
      keyd
      clang
      gcc
      dmenu
    ]
    ++ (
      if localconfig.install.awesomewm
      then [inputs.nixpkgs-f2k.packages.${pkgs.system}.awesome-git]
      else []
    );

  services.xserver.displayManager.startx.enable = true;
  services.xserver.displayManager.session =
    []
    ++ (
      if localconfig.install.awesomewm
      then [
        {
          manage = "desktop";
          name = "awesome";
          start = ''
            ${inputs.nixpkgs-f2k.packages.${pkgs.system}.awesome-git}/bin/awesome &
            waitPID=$!
          '';
        }
      ]
      else []
    )
    ++ (
      if localconfig.install.awesomewm
      then [
        {
          manage = "desktop";
          name = "hyprland";
          start = ''
            ${pkgs.hyprland}/bin/Hyprland &
            waitPID=$!
          '';
        }
      ]
      else []
    );

  services.keyd = {
    enable = true;
    keyboards.default.ids = [
      "*"
    ];
    keyboards.default.settings = {
      main = {
        capslock = "overload(meta, esc)";
      };
    };
  };

  services.openssh.enable = true;
}
