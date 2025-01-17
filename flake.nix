{
  description = "nixos";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://hyprland.cachix.org"
      "https://cosmic.cachix.org/"
      "https://chaotic-nyx.cachix.org/"
      "https://cache.iog.io" # haskell.nix
      "https://nix-community.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # default nixos TODO useless?
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" # hyprland
      "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE=" # cosmic
      "chaotic-nyx.cachix.org-1:HfnXSw4pj95iI/n17rIDy40agHj12WfF+Gqk6SonIT8=" # chaotic nyx
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # haskell.nix
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" # nix-community
    ];
  };

  outputs = {self, ...} @ inputs: let
    # hybrid flake + npins
    # npins = import ./npins/wrapper.nix;
    # inputs = inputs' // npins;
    lib = inputs.nixpkgs.lib;
    mlib = (import ./lib) {inherit lib;};
    mpkgs = import ./pkgs;
  in rec {
    formatter.x86_64-linux = inputs.nixpkgs.legacyPackages.x86_64-linux.alejandra;

    deploy.nodes = {
      server = {
        hostname = "192.168.101.101";
        profiles.system = {
          user = "root";
          sshUser = "root";
          path = inputs.deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.server;
        };
      };
    };

    checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) inputs.deploy-rs.lib;

    nixosConfigurations =
      gen ["desktop" "server" "x220" "t440p" "digiboksi"]
      // {
        # iso = (import ./iso) {inherit lib inputs;};
        # iso = mkSystem (import ./hosts/iso.nix) [
        #   "${inputs.nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares.nix"
        # ];
      };

    nixosModules.default = {...}: {
      imports = ./modules;
    };

    gen = hosts:
      builtins.listToAttrs (builtins.map
        (s: {
          name = s;
          # lib/os.nix
          value = mlib.mkSystem {
            nixosSystem = lib.nixosSystem;
            inherit inputs mlib;
            config = mlib.getHostConfig s;
          };
        })
        hosts);
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-24-05.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-24-11.url = "github:NixOS/nixpkgs/nixos-24.11";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    deploy-rs.url = "github:serokell/deploy-rs";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.1-2.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    cosmic = {
      url = "github:lilyinstarlight/nixos-cosmic";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    stylix.url = "github:danth/stylix";
    stylix.inputs.home-manager.follows = "home-manager";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-xr.url = "github:nix-community/nixpkgs-xr";

    rust-overlay.url = "github:oxalica/rust-overlay";

    # screenshot.url = "./pkgs/screenshot";

    naersk.url = "github:nix-community/naersk";
    # haskell-nix.url = "github:input-output-hk/haskell.nix";

    hyprland = {
      # latest before clipboard break
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=4cdddcfe466cb21db81af0ac39e51cc15f574da9";
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
      # url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=9a09eac79b85c846e3a865a9078a3f8ff65a9259";
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1";
    };

    waybar.url = "github:Alexays/Waybar";

    hyprsplit = {
      url = "github:shezdy/hyprsplit";
      inputs.hyprland.follows = "hyprland";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nix-gaming.url = "github:fufexan/nix-gaming";

    nur.url = "github:nix-community/NUR";

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    sops-nix.url = "github:Mic92/sops-nix";
  };
}
