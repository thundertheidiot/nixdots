{inputs, ...}: {
  config = {
    nixpkgs.overlays = [
      (final: prev: rec {
        # TODO organize, break up
        nur = import inputs.nur {
          nurpkgs = prev;
          pkgs = prev;
        };
        firefox-addons = nur.repos.rycee.firefox-addons;
        ataraxiasjel = nur.repos.ataraxiasjel;
        hyprsplit = inputs.hyprsplit.packages.${final.stdenv.hostPlatform.system}.default;

        sodexobot = inputs.sodexobot.packages.${final.stdenv.hostPlatform.system}.default;
        leptos-kotiboksi = inputs.leptos-kotiboksi.packages.${final.stdenv.hostPlatform.system}.default;
        meowdzbot = inputs.meowdzbot.packages.${final.stdenv.hostPlatform.system}.default;

        unstable = import inputs.nixpkgs-unstable {
          inherit (final) config;
          system = final.stdenv.hostPlatform.system;
        };

        # unstable-small occasionally results in electron rebuilds
        inherit (final.unstable) element-desktop libreoffice qmk avr-gcc;

        mpkgs = (import "${inputs.self.outPath}/pkgs") {pkgs = final;};

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
