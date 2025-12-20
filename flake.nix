{
  description = "meowos";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://hyprland.cachix.org"
      "https://nix-community.cachix.org"
      "https://meowos.cachix.org" # meowos binary cache
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # default nixos TODO useless?
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" # hyprland
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" # nix-community
      "meowos.cachix.org-1:QOXuuFPMN5TszgX8+nqd8X+BZG84toh5wK8j1IBBDH4="
    ];
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake {inherit inputs;} {imports = [./flake];};

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable-small/nixexprs.tar.xz";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-25-05.url = "github:NixOS/nixpkgs/nixos-25.05";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    # not sure if aly's fork does much, but it was apparently important for her
    # https://github.com/nialov/actions.nix/compare/master...alyraffauf:actions.nix:master
    actions.url = "github:alyraffauf/actions.nix";

    pkgs-by-name.url = "github:drupol/pkgs-by-name-for-flake-parts";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    deploy-rs.url = "github:serokell/deploy-rs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs.url = "github:thundertheidiot/emacs";
    emacs.inputs.nixpkgs.follows = "nixpkgs";

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    stylix.url = "github:danth/stylix";
    # stylix.inputs.home-manager.follows = "home-manager";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-xr.url = "github:nix-community/nixpkgs-xr";

    rust-overlay.url = "github:oxalica/rust-overlay";

    naersk.url = "github:nix-community/naersk";

    hyprland = {
      url = "github:hyprwm/Hyprland/v0.51.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprsplit = {
      url = "github:shezdy/hyprsplit/v0.51.1";
      inputs.hyprland.follows = "hyprland";
    };

    hyprlux.url = "github:amadejkastelic/Hyprlux";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nur.url = "github:nix-community/NUR";

    sops-nix.url = "github:Mic92/sops-nix";

    # server
    authentik-nix.url = "github:nix-community/authentik-nix";
    nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/master";
    sodexobot = {
      url = "github:thundertheidiot/sodexobot";
    };
    leptos-kotiboksi = {
      url = "github:thundertheidiot/leptos-kotiboksi";
    };
    meowdzbot = {
      url = "github:thundertheidiot/meowdzbot";
    };
  };
}
