{
  description = "meowos";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://hyprland.cachix.org"
      "https://chaotic-nyx.cachix.org/"
      "https://cache.iog.io" # haskell.nix
      "https://nix-community.cachix.org"
      "https://meowos.cachix.org" # meowos binary cache
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # default nixos TODO useless?
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" # hyprland
      "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8=" # chaotic nyx
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # haskell.nix
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" # nix-community
      "meowos.cachix.org-1:QOXuuFPMN5TszgX8+nqd8X+BZG84toh5wK8j1IBBDH4="
    ];
  };

  # outputs = {self, ...} @ inputs: let
  #   lib = inputs.nixpkgs.lib;
  #   mlib = (import ./lib) {inherit lib;};
  # in rec {
  #   formatter.x86_64-linux = inputs.nixpkgs.legacyPackages.x86_64-linux.alejandra;

  #   deploy.nodes = {
  #     server = {
  #       hostname = "192.168.101.101";
  #       profiles.system = {
  #         user = "root";
  #         sshUser = "root";
  #         path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.server;
  #       };
  #     };
  #   };

  #   checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;

  #   nixosConfigurations =
  #     gen ["desktop" "server" "x220" "t440p" "digiboksi"]
  #     // {
  #       iso = (import ./iso) {inherit mlib lib inputs;};
  #     };

  #   gen = hosts:
  #     builtins.listToAttrs (builtins.map
  #       (s: {
  #         name = s;
  #         # lib/os.nix
  #         value = mlib.mkSystem {
  #           nixosSystem = lib.nixosSystem;
  #           inherit inputs mlib;
  #           config = mlib.getHostConfig s;
  #         };
  #       })
  #       hosts);
  # };

  outputs = inputs: inputs.flake-parts.lib.mkFlake {inherit inputs;} (inputs.import-tree ./modules);

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-unstable-small/nixexprs.tar.xz";
    nixpkgs-24-11.url = "github:NixOS/nixpkgs/nixos-24.11";
    nixpkgs-25-05.url = "github:NixOS/nixpkgs/nixos-25.05";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    import-tree.url = "github:vic/import-tree";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix.url = "github:danth/stylix";
    # stylix.inputs.home-manager.follows = "home-manager";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-xr.url = "github:nix-community/nixpkgs-xr";

    rust-overlay.url = "github:oxalica/rust-overlay";

    # screenshot.url = "./pkgs/screenshot";

    naersk.url = "github:nix-community/naersk";

    hyprland = {
      # inputs.nixpkgs.follows = "nixpkgs";
      # inputs.nixpkgs.follows = "nixpkgs";
      # url = "github:hyprwm/Hyprland/v0.46.2";
      url = "github:hyprwm/Hyprland/v0.50.1";
    };

    hyprsplit = {
      url = "github:shezdy/hyprsplit/v0.50.1";
      inputs.hyprland.follows = "hyprland";
    };

    hyprlux.url = "github:amadejkastelic/Hyprlux";

    # waybar.url = "github:Alexays/Waybar";

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nix-gaming.url = "github:fufexan/nix-gaming";

    nur.url = "github:nix-community/NUR";
    authentik-nix.url = "github:nix-community/authentik-nix";

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    sops-nix.url = "github:Mic92/sops-nix";
  };
}
