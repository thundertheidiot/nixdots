{
  description = "meowos";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://meowos.cachix.org" # meowos binary cache
      "https://vicinae.cachix.org"
      "https://attic.xuyh0120.win/lantian" # cachyos kernel
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # default nixos TODO useless?
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" # nix-community
      "meowos.cachix.org-1:QOXuuFPMN5TszgX8+nqd8X+BZG84toh5wK8j1IBBDH4="
      "vicinae.cachix.org-1:1kDrfienkGHPYbkpNj1mWTr7Fm1+zcenzgTizIcI3oc="
      "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc="
    ];
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake {inherit inputs;} {imports = [./flake];};

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable-small/nixexprs.tar.xz";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

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

    nix-cachyos-kernel.url = "github:xddxdd/nix-cachyos-kernel";

    deploy-rs.url = "github:serokell/deploy-rs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xrizer = {
      url = "github:thundertheidiot/xrizer/alien-isolation-fix";
      flake = false;
    };

    emacs.url = "github:thundertheidiot/emacs";
    emacs.inputs.nixpkgs.follows = "nixpkgs";

    catppuccin.url = "github:catppuccin/nix";

    nixpkgs-xr.url = "github:nix-community/nixpkgs-xr";

    rust-overlay.url = "github:oxalica/rust-overlay";

    naersk.url = "github:nix-community/naersk";

    vicinae.url = "github:vicinaehq/vicinae";

    nur.url = "github:nix-community/NUR";

    sops-nix.url = "github:Mic92/sops-nix";

    # server
    authentik-nix.url = "github:nix-community/authentik-nix";
    nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/main";
    autoaspm = {
      url = "git+https://git.notthebe.ee/notthebee/AutoASPM";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
