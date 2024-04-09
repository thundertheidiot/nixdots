{
  description = "nixos";

  outputs = {
    self,
    nixpkgs,
    home-manager,
    mobile-nixos,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
  in rec {
    # packages."aarch64-linux".fajita = (import "${mobile-nixos}/lib/eval-with-configuration.nix" {
    #   device = "oneplus-fajita";
    #   pkgs = (import "${mobile-nixos}/pkgs.nix");
    #   configuration = import ./modules/mobile/fajita.nix;
    # });

    nixosConfigurations = gen ["desktop" "x220" "t440p" "digiboksi"] // {
      local = mkSystem (import ./local.nix) [];
      # iso = mkSystem (import ./hosts/iso.nix) [
      #   "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-graphical-calamares.nix"
      # ];
      fajita = let
        lib = (import "${mobile-nixos}/pkgs.nix").lib;
      in lib.nixosSystem {
        system = "aarch64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          (import "${mobile-nixos}/lib/config/configuration.nix" { device = "oneplus-fajita"; })
          ./modules/mobile/fajita.nix
        ];
      };
    };

    # defaultPackage = builtins.listToAttrs (builtins.map (arch: {
    #   name = arch;
    #   value = home-manager.defaultPackage.arch;
    # }) ["linux-x86_64" "aarch64-linux" "i686-linux"]);

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
        specialArgs = {inherit inputs;};
        modules =
          common
          ++ extramodules
          ++ [
            cfg.system
            home-manager.nixosModules.home-manager
            inputs.agenix.nixosModules.default
            inputs.chaotic.nixosModules.default
            ({config, ...}: {
              time.timeZone = config.timeZone;
              networking.hostName = config.hostName;

              nixpkgs = {
                system = config.systemArch;
                config.allowUnfree = true;
                overlays = [
                  inputs.emacs-overlay.overlay
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
                    agenix = inputs.agenix.packages.${final.system};
                    hyprland = inputs.hyprland.packages.${final.system}.hyprland;
                    awesome = inputs.nixpkgs-f2k.packages.${final.system}.awesome-git;
                  })
                ];
              };

              imports = [
                ./modules/nixos
              ];

              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = {inherit inputs;};

                sharedModules =
                  common
                  ++ [
                    cfg.home
                    inputs.agenix.homeManagerModules.default
                  ];

                users.${config.username} = ./modules/home;
              };
            })
          ];
      };

    gen = hosts:
      builtins.listToAttrs (builtins.map (s: {
          name = s;
          value = mkSystem (import ./hosts/${s}.nix) [];
        })
        hosts);
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-23-11.url = "github:NixOS/nixpkgs/nixos-23.11";

    mobile-nixos = {
      url = "github:NixOS/mobile-nixos";
      flake = false;
    };

    home-manager = {
      url = "github:nix-community/home-manager";
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
      # url = "github:hyprwm/Hyprland?ref=e1e41e54480282d9bec9957d3c578eb87bc1f2f2";
      url = "github:hyprwm/Hyprland";
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
