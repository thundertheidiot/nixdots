{
  description = "nixos";

  outputs = {
    self,
    nixpkgs,
    home-manager,
    mobile-nixos,
    hyprland,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
    mlib = (import ./lib) {inherit lib;};
  in rec {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;

    nixosConfigurations =
      gen ["desktop" "x220" "t440p" "digiboksi"]
      // {
        local = mkSystem (import ./local.nix) [];
        # iso = mkSystem (import ./hosts/iso.nix) [
        #   "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares.nix"
        # ];
      };

    commonModules = cfg: [
      ./options.nix
      cfg.options
      ({config, ...}: {
        age.identityPaths = ["${config.homeDirectory}/.ssh/id_agenix"];
        age.secrets.kodi_youtube_api_keys.file = ./secrets/kodi_youtube_api_keys.age;
      })
    ];

    mkSystem = cfg: extramodules: let
      common = commonModules cfg;
    in
      lib.nixosSystem {
        system = cfg.systemArch;
        specialArgs = {inherit inputs mlib;};
        modules =
          common
          ++ extramodules
          ++ [
            cfg.system
            home-manager.nixosModules.home-manager
            inputs.agenix.nixosModules.default
            inputs.chaotic.nixosModules.default
            ({
              config,
              lib,
              ...
            }: {
              time.timeZone = config.timeZone;
              networking.hostName = config.hostName;

              nix.settings = {
                substituters = [
                  "https://hyprland.cachix.org"
                  "https://cosmic.cachix.org/"
                ];
                trusted-public-keys = [
                  "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
                  "cosmic.cachix.org-1:Dya9IyXD4xdBehWjrkPv6rtxpmMdRel02smYzA85dPE="
                ];
              };

              nixpkgs = {
                system = config.systemArch;
                config.allowUnfree = true;
                overlays = [
                  inputs.emacs-overlay.overlay
                  # inputs.hyprland.overlays.default
                  inputs.waybar.overlays.default
                  (final: prev: rec {
                    nur = import inputs.nur {
                      nurpkgs = prev;
                      pkgs = prev;
                    };
                    firefox-addons = nur.repos.rycee.firefox-addons;
                    ataraxiasjel = nur.repos.ataraxiasjel;
                    "2311" = import inputs.nixpkgs-23-11 {
                      system = final.system;
                      config.allowUnfree = final.config.allowUnfree;
                    };
                    "2405" = import inputs.nixpkgs-24-05 {
                      system = final.system;
                      config.allowUnfree = final.config.allowUnfree;
                    };
                    agenix = inputs.agenix.packages.${final.system};
                    swayfx = inputs.swayfx.packages.${final.system}.swayfx-unwrapped;
                    awesome = inputs.nixpkgs-f2k.packages.${final.system}.awesome-git;

                    hyprland = inputs.hyprland.packages.${final.system}.hyprland;
                    xdg-desktop-portal-hyprland = inputs.hyprland.packages.${final.system}.xdg-desktop-portal-hyprland;
                    hyprland-protocols = inputs.hyprland.packages.${final.system}.hyprland-protocols;
                    wlroots-hyprland = inputs.hyprland.packages.${final.system}.wlroots-hyprland;

                    hyprland-split-monitor-workspaces = inputs.split-monitor-workspaces.packages.${final.system}.split-monitor-workspaces;
                    # udis86 = inputs.hyprland.packages.${final.system}.udis86;
                  })
                ];
              };

              imports = [
                inputs.lix-module.nixosModules.default # lix
                inputs.cosmic.nixosModules.default
                inputs.hyprland.nixosModules.default
                ./modules/nixos
              ];

              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = {inherit inputs mlib;};

                backupFileExtension = "hm_backup";

                sharedModules =
                  common
                  ++ [
                    cfg.home
                    inputs.agenix.homeManagerModules.default
                    inputs.plasma-manager.homeManagerModules.plasma-manager
                  ];

                users.${config.username} = ./modules/home;
              };
            })
          ];
      };

    gen = hosts:
      builtins.listToAttrs (builtins.map
        (s: {
          name = s;
          value = mkSystem (import ./hosts/${s}.nix) [];
        })
        hosts);
  };

  inputs = {
    # https://hydra.nixos.org/job/nixpkgs/trunk/unstable
    # https://status.nixos.org/
    # nixpkgs.url = "github:NixOS/nixpkgs?ref=7bb2ccd8cdc44c91edba16c48d2c8f331fb3d856";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-24-05.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixpkgs-23-11.url = "github:NixOS/nixpkgs/nixos-23.11";

    naersk.url = "github:nix-community/naersk";

    lix-module = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.90.0-rc1.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mobile-nixos = {
      url = "github:NixOS/mobile-nixos";
      flake = false;
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    plasma-manager = {
      url = "github:pjones/plasma-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cosmic = {
      url = "github:lilyinstarlight/nixos-cosmic";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    base16.url = "github:SenchoPens/base16.nix";
    tt-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    nixpkgs-f2k.url = "github:moni-dz/nixpkgs-f2k";

    hyprland = {
      # Gpu works, split monitor workspaces works, idk
      # Old ass
      # url = "github:hyprwm/Hyprland?ref=e1e41e54480282d9bec9957d3c578eb87bc1f2f2";
      # 0.38. whatevevr works
      # url = "github:hyprwm/Hyprland?ref=303b9956b2ae15508b09dffae602550ca17e6539";
      # latest before clipboard break
      url = "git+https://github.com/hyprwm/Hyprland?submodules=1&rev=4cdddcfe466cb21db81af0ac39e51cc15f574da9";
    };

    waybar.url = "github:Alexays/Waybar";

    swayfx = {
      url = "github:WillPower3309/swayfx";
    };

    split-monitor-workspaces = {
      # for the ref above
      # url = "github:thundertheidiot/split-monitor-workspaces?ref=d6efa8bd7254f40a2009d406934059fda539811d";
      url = "github:thundertheidiot/split-monitor-workspaces";
      inputs.hyprland.follows = "hyprland";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    nixgl.url = "github:nix-community/nixGL";
    nix-gaming.url = "github:fufexan/nix-gaming";

    nur.url = "github:nix-community/NUR";

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";

    agenix.url = "github:ryantm/agenix";
  };
}
