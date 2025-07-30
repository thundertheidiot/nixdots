{
  config,
  inputs,
  ...
}: {
  flake.modules.nixos.base = {
    nixpkgs.overlays = [
      (final: prev: rec {
        # TODO organize, break up
        nur = import inputs.nur {
          nurpkgs = prev;
          pkgs = prev;
        };
        firefox-addons = nur.repos.rycee.firefox-addons;
        ataraxiasjel = nur.repos.ataraxiasjel;
        hyprsplit = inputs.hyprsplit.packages.${final.system}.default;

        "2411" = import inputs.nixpkgs-24-11 {
          system = final.system;
          config.allowUnfree = final.config.allowUnfree;
        };

        "2505" = import inputs.nixpkgs-25-05 {
          system = final.system;
          config.allowUnfree = final.config.allowUnfree;
        };

        mpkgs = (import "${config.flake.root}/pkgs") {pkgs = final;};

        avrdude = prev.avrdude.overrideAttrs (old: {
          src = old.src.override {
            repo = "avrdude";
          };
        });

        gnome2 =
          prev.gnome2
          // {
            # ???????????????
            libglade = final."2505".gnome2.libglade;
          };

        mpd = final."2505".mpd;

        vulkan-validation-layers = prev.vulkan-validation-layers.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [final.spirv-tools];
        });

        # functionality fixes
        gajim = prev.gajim.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ [final.makeWrapper];

          # fix gnome-keyring on kde
          postInstall =
            old.postInstall
            + ''
              wrapProgram $out/bin/gajim --set XDG_CURRENT_DESKTOP GNOME
            '';
        });

        rathole = prev.rathole.overrideAttrs (old: {
          doCheck = false;
        });

        mumble = prev.mumble.overrideAttrs (old: {
          postFixup =
            builtins.replaceStrings
            ["wrapProgram $out/bin/mumble"]
            ["wrapProgram $out/bin/mumble --set QT_QPA_PLATFORM xcb"] # Run with xwayland to make keybindings work
            
            old.postFixup;
        });
      })
    ];
  };
}
