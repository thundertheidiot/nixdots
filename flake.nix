{
  description = "nixos";

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    lib = nixpkgs.lib;
  in rec {
    nixosConfigurations = gen ["desktop" "x220" "digiboksi"] // {
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

                    ccacheWrapper = prev.ccacheWrapper.override {
                      extraConfig = ''
                        export CCACHE_COMPRESS=1
                        export CCACHE_DIR="${config.programs.ccache.cacheDir}"
                        export CCACHE_UMASK=007
                        if [ ! -d "$CCACHE_DIR" ]; then
                          echo "====="
                          echo "Directory '$CCACHE_DIR' does not exist"
                          echo "Please create it with:"
                          echo "  sudo mkdir -m0770 '$CCACHE_DIR'"
                          echo "  sudo chown root:nixbld '$CCACHE_DIR'"
                          echo "====="
                          exit 1
                        fi
                        if [ ! -w "$CCACHE_DIR" ]; then
                          echo "====="
                          echo "Directory '$CCACHE_DIR' is not accessible for user $(whoami)"
                          echo "Please verify its access permissions"
                          echo "====="
                          exit 1
                        fi
                      '';
                    };
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
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-23-11.url = "github:NixOS/nixpkgs/nixos-23.11";

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
      url = "github:hyprwm/Hyprland?ref=v0.37.1";
    };

    split-monitor-workspaces = {
      url = "github:Dekomoro/split-monitor-workspaces";
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
