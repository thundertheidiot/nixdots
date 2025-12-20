{
  config,
  lib,
  mlib,
  pkgs,
  ...
}: let
  inherit (mlib) mkOpt;
  inherit (lib.types) enum listOf;
  inherit (lib) mkIf mkMerge;

  cfg = config.meow.workstation.enable;
  envir = config.meow.workstation.environment;
  dm = config.meow.workstation.displayManager;
in {
  options = {
    meow.workstation.environment = mkOpt (listOf (enum ["hyprland" "niri"])) [] {
      description = "The list of environments to configure and install.";
    };

    meow.workstation.displayManager = mkOpt (enum ["sddm" "gdm"]) "sddm" {
      description = "Display manager (login screen) to install.";
    };

    # meow.workstation.xdgPortals = mkOpt (listOf package) [] {
    #   description = "Do not touch, internal way of passing values.";
    # };
  };
  imports = [
    ./hyprland
    ./niri
  ];

  config = mkIf cfg (mkMerge [
    {
      xdg.portal = {
        enable = true;
        xdgOpenUsePortal = true;
      };
    }
    {
      programs.niri.enable = lib.mkDefault false;
    }
    (mkIf (dm == "sddm") {
      services.displayManager.sddm = {
        enable = true;
        package = lib.mkForce pkgs.kdePackages.sddm;
        wayland = {
          enable = true;
          # compositor = lib.mkForce "weston";
        };
      };

      meow.impermanence.directories = [
        {
          path = "/var/lib/sddm";
          permissions = "750";
          user = "sddm";
          group = "sddm";
        }
      ];
    })
    (mkIf (dm == "gdm") {
      services.displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    })
    (mkIf (builtins.elem "hyprland" envir) {
      services.gvfs.enable = true;

      systemd.user.services.polkit-gnome-authentication-agent-1 = {
        description = "polkit-gnome-authentication-agent-1";
        wantedBy = ["graphical-session.target"];
        wants = ["graphical-session.target"];
        after = ["graphical-session.target"];
        serviceConfig = {
          Type = "simple";
          ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
          Restart = "on-failure";
          RestartSec = 1;
          TimeoutStopSec = 10;
        };
      };
    })
    {
      # services.cpupower-gui.enable = true;

      meow.home.modules = [
        ({config, ...}: {
          xresources = {
            path = "${config.xdg.configHome}/xresources";
          };
        })
      ];
    }
  ]);
}
