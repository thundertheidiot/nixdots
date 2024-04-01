{
  lib,
  config,
  pkgs,
  localconfig,
  inputs,
  ...
}: {
  imports = [
    inputs.nix-gaming.nixosModules.pipewireLowLatency
    ./hyprland.nix
  ];
  config = lib.mkIf (config.setup.userMachine.enable) (with config; {
    environment.systemPackages = with pkgs; [
      dmenu
      dconf
      gnome.gnome-keyring
      (pkgs.stdenv.mkDerivation {
        name = "sddm-catppuccin-mocha";

        src = fetchzip {
          url = "https://github.com/catppuccin/sddm/releases/download/v1.0.0/catppuccin-mocha.zip";
          hash = "sha256-+YxKzuu2p46QoCZykXLYFwkXcJ+uJ7scwDU7vJ4b1pA=";
        };

        dontConfigure = true;
        dontBuild = true;

        installPhase = ''
          runHook preInstall

          mkdir -p "$out/share/sddm/themes/"
          cp -r "$src" "$out/share/sddm/themes/catppuccin-mocha"

          runHook postInstall
        '';
      })
    ];

    services.gnome.gnome-keyring.enable = true;

    programs.seahorse.enable = true;

    security.pam.services.gnome-keyring = {
      name = "gnome-keyring";
      enableGnomeKeyring = true;
      text = ''
        auth optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so
        session optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so auto_start
        password optional ${pkgs.gnome.gnome-keyring}/lib/security/pam_gnome_keyring.so
      '';
    };

    services.xserver.enable = true;
    services.xserver.displayManager.sddm = {
      enable = true;
      theme = "catppuccin-mocha";
    };

    security.rtkit.enable = true;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      lowLatency = {
        enable = true;
      };
    };

    services.xserver.displayManager.startx.enable = true;

    services.xserver.displayManager.session = [
      (lib.mkIf (config.setup.awesomeWM.enable) {
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
        lib.mkIf (config.setup.hyprland.enable)
        inputs.hyprland.packages.${pkgs.system}.hyprland
      )
    ];
  });
}
