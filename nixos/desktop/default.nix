{
  lib,
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}:
with config;
  lib.mkIf (localconfig.install.desktop) {
    environment.systemPackages = with pkgs; [
      dmenu
      dconf
    ];

    services.xserver.enable = true;
    services.xserver.displayManager.lightdm = {
      enable = true;
    };

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      # lowLatency = {
      #   enable = true;
      # };
    };

    services.xserver.displayManager.startx.enable = true;

    services.xserver.displayManager.session = [
      (lib.mkIf (localconfig.install.awesomewm) {
        manage = "desktop";
        name = "awesome";
        start = ''
          ${inputs.nixpkgs-f2k.packages.${pkgs.system}.awesome-git}/bin/awesome &
          waitPID=$!
        '';
      })
    ];

    services.xserver.displayManager.sessionPackages = [
      (
        lib.mkIf (localconfig.install.hyprland)
        inputs.hyprland.packages.${pkgs.system}.hyprland
      )
    ];
  }
