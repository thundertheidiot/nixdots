{
  lib,
  inputs,
}: (final: prev: rec {
  nur = import inputs.nur {
    nurpkgs = prev;
    pkgs = prev;
  };
  firefox-addons = nur.repos.rycee.firefox-addons;
  ataraxiasjel = nur.repos.ataraxiasjel;
  hyprland-split-monitor-workspaces = inputs.split-monitor-workspaces.packages.${final.system}.split-monitor-workspaces;

  "2405" = import inputs.nixpkgs-24-05 {
    system = final.system;
    config.allowUnfree = final.config.allowUnfree;
  };

  # TODO: infinite todo: build fixes
  avrdude = prev.avrdude.overrideAttrs (old: {
    src = old.src.override {
      repo = "avrdude";
    };
  });

  # envision = prev.envision.override {
  #   envision-unwrapped = prev.envision-unwrapped.overrideAttrs (old: {
  #     src = final.fetchFromGitLab {
  #       owner = "gabmus";
  #       repo = "envision";
  #       rev = "91f61e5c4ae91eb21167ad7f649b0223ebcc17a3";
  #       hash = "sha256-GYl6sNfW7sTR5os6ysumlg078w8iBcUU15t0x7Gr+AE=";
  #     };
  #   });
  # };

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

  mumble = prev.mumble.overrideAttrs (old: {
    postFixup =
      builtins.replaceStrings
      ["wrapProgram $out/bin/mumble"]
      ["wrapProgram $out/bin/mumble --set QT_QPA_PLATFORM xcb"] # Run with xwayland to make keybindings work
      
      old.postFixup;
  });
})
