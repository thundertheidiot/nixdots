{
  description = "meowos";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://nix-community.cachix.org"
      "https://meowos.cachix.org" # meowos binary cache
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # default nixos TODO useless?
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" # nix-community
      "meowos.cachix.org-1:QOXuuFPMN5TszgX8+nqd8X+BZG84toh5wK8j1IBBDH4="
    ];
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } { imports = [ ./flake ]; };

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable-small/nixexprs.tar.xz";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs-multiverse.url = "github:fzakaria/nixpkgs-multiverse";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    # not sure if aly's fork does much, but it was apparently important for her
    # https://github.com/nialov/actions.nix/compare/master...alyraffauf:actions.nix:master
    actions.url = "github:alyraffauf/actions.nix";

    albedo.url = "git+https://tangled.org/okami.mom/albedo";

    review-artifact.url = "github:thundertheidiot/review-artifact";
    review-artifact.inputs.nixpkgs.follows = "nixpkgs";

    comin = {
      url = "github:nlewo/comin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixwine.url = "git+https://tangled.org/evil.nix.ee/nix-wine";

    sable.url = "git+https://git.sable.moe/SableClient/sable-next";
    sable.inputs.nixpkgs.follows = "nixpkgs";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs.url = "github:thundertheidiot/emacs";
    emacs.inputs.nixpkgs.follows = "nixpkgs";

    catppuccin.url = "github:catppuccin/nix";

    nixpkgs-xr.url = "github:nix-community/nixpkgs-xr";
    nixpkgs-xr.inputs.nixpkgs.follows = "nixpkgs";

    rust-overlay.url = "github:oxalica/rust-overlay";

    naersk.url = "github:nix-community/naersk";

    nur.url = "github:nix-community/NUR";

    sops-nix.url = "github:Mic92/sops-nix";

    noctalia.url = "github:noctalia-dev/noctalia";
    noctalia.inputs.nixpkgs.follows = "nixpkgs";

    # server
    nixflix = {
      url = "github:kiriwalawren/nixflix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/main";
    sodexobot = {
      url = "github:thundertheidiot/sodexobot";
    };
    leptos-kotiboksi = {
      url = "github:thundertheidiot/leptos-kotiboksi";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    meowdzbot = {
      url = "github:thundertheidiot/meowdzbot";
    };
    kissasome = {
      url = "github:thundertheidiot/kissasome";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-overlay.follows = "rust-overlay";
    };
  };
}
