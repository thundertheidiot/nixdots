{
  mlib,
  lib,
  inputs,
  ...
}: let
  config = {
    networking.hostName = "meow-iso";
    system.stateVersion = "24.11";

    # NetworkManager
    networking.wireless.enable = false;

    nixpkgs.hostPlatform = "x86_64-linux";

    users.users.nixos.initialPassword = "nixos";

    meow = {
      workstation = {
        enable = true;
        environment = ["gnome"];
        displayManager = "gdm";
      };
      workstation.flatpak.enable = false;

      shell.enable = true;

      emacs.enable = true;

      ssh.key = true;
      ssh.rootKey = true;

      home = {
        user = "nixos";
        stateVersion = "24.11";
        modules = [
          {
            home.file."host.nix".source = ./templates/host.nix;
            home.file."disko.nix".source = ./templates/disko.nix;
          }
        ];
      };
    };
  };
in
  mlib.mkSystem {
    nixosSystem = lib.nixosSystem;
    inherit mlib inputs config;
    extraModules = [
      "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-base.nix"
      "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
    ];
  }
