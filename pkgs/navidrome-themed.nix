# navidrome with its catppuccin mocha theme recoloured to a desktop palette
{
  lib,
  navidrome,
  navidrome-unwrapped,
  themeName,
  roles,
}:
let
  inherit (lib) concatStringsSep mapAttrsToList;

  # catppuccin mocha hexes used by the upstream theme files
  swaps = {
    "#1e1e2e" = roles.background;
    "#181825" = roles.panel;
    "#313244" = roles.surface;
    "#45475a" = roles.border;
    "#9399b2" = roles.subtle;
    "#a6adc8" = roles.muted;
    "#cdd6f4" = roles.foreground;
    "#cba6f7" = roles.accent;
    "#89b4fa" = roles.info;
    "#f38ba8" = roles.error;
  };
  sedArgs = concatStringsSep " " (mapAttrsToList (from: to: "-e 's/${from}/${to}/gI'") swaps);
in
navidrome.override {
  navidrome-unwrapped = navidrome-unwrapped.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + ''
      pushd ui/src/themes
      sed ${sedArgs} \
        -e "s|catppuccinMocha.css.js|desktop.css.js|" \
        -e "s|themeName: 'Catppuccin Mocha'|themeName: '${themeName}'|" \
        catppuccinMocha.js > desktop.js
      sed ${sedArgs} catppuccinMocha.css.js > desktop.css.js
      substituteInPlace index.js --replace-fail "export default {" \
        "import DesktopTheme from './desktop'
      export default {
        DesktopTheme,"
      popd
    '';
  });
}
