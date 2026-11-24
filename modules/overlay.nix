{
  inputs,
  ...
}:
{
  config = {
    nixpkgs.overlays = [
      inputs.nixwine.overlays.default

      (inputs.nixpkgs-multiverse.lib.pinOverlay {
        pins.opencode = "1.18.29";
      })

      (final: prev: rec {
        # TODO organize, break up
        nur = import inputs.nur {
          nurpkgs = prev;
          pkgs = prev;
        };
        firefox-addons = nur.repos.rycee.firefox-addons;

        unstable = import inputs.nixpkgs-unstable {
          inherit (final) config;
          system = final.stdenv.hostPlatform.system;
        };

        inherit (final.unstable)
          element-desktop
          libreoffice
          qmk
          avr-gcc
          kdenlive
          blender
          krita
          firefox
          ananicy-cpp
          flaresolverr
          ;

        sable = inputs.sable.packages.${final.stdenv.hostPlatform.system}.sable;
        sable-desktop =
          inputs.sable.packages.${final.stdenv.hostPlatform.system}.sable-desktop.overrideAttrs
            (old: {
              # upstream packaging misses these native deps
              buildInputs = old.buildInputs ++ [
                final.openssl
                final.sqlcipher
              ];
            });

        sodexobot = inputs.sodexobot.packages.${final.stdenv.hostPlatform.system}.default;
        leptos-kotiboksi = inputs.leptos-kotiboksi.packages.${final.stdenv.hostPlatform.system}.default;
        meowdzbot = inputs.meowdzbot.packages.${final.stdenv.hostPlatform.system}.default;
        kissasome =
          inputs.kissasome.packages.${final.stdenv.hostPlatform.system}.default.overrideAttrs
            (old: {
              buildInputs = (old.buildInputs or [ ]) ++ [ final.zstd ];
              nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ final.removeReferencesTo ];
              # toolchain paths leak into the binary, drop them from the closure
              postFixup = (old.postFixup or "") + ''
                for p in $(grep -aoE '/nix/store/[a-z0-9]{32}-rust[^/" ]*' $out/bin/.kissasome-wrapped | sort -u); do
                  remove-references-to -t $p $out/bin/.kissasome-wrapped
                done
              '';
            });

        mpkgs = (import "${inputs.self.outPath}/pkgs") { pkgs = final; };

        # functionality fixes
        tmc-cli = prev.tmc-cli.overrideAttrs (old: {
          nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ final.pkg-config ];
          buildInputs = (old.buildInputs or [ ]) ++ [ final.zstd ];
        });

        gajim = prev.gajim.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ [ final.makeWrapper ];

          # fix gnome-keyring on kde
          postInstall = old.postInstall + ''
            wrapProgram $out/bin/gajim --set XDG_CURRENT_DESKTOP GNOME
          '';
        });

        mumble = prev.mumble.overrideAttrs (old: {
          postFixup =
            builtins.replaceStrings
              [ "wrapProgram $out/bin/mumble" ]
              [ "wrapProgram $out/bin/mumble --set QT_QPA_PLATFORM xcb" ] # Run with xwayland to make keybindings work

              old.postFixup;
        });
      })

      (
        final: prev:
        let
          wrapHome =
            pkg:
            {
              name ? pkg.meta.name or pkg.name,
              executable ? pkg.meta.mainProgram or pkg.name,
            }:
            final.symlinkJoin {
              name = "${name}-wrapped-home";
              paths = [ pkg ];
              buildInputs = [ final.makeWrapper ];

              postBuild = "wrapProgram $out/bin/${executable} --run 'export HOME=\"\${STUBBORN_HOME_DIRECTORY:-$HOME}\"'";
            };
        in
        {
          lmath = wrapHome prev.lmath { };
          cloudflared = wrapHome prev.cloudflared { };
          android-tools = wrapHome prev.android-tools { executable = "adb"; };
          signal-desktop = wrapHome prev.signal-desktop { };
        }
      )
    ];
  };
}
