{ ... }:
let
  inherit (builtins)
    elemAt
    fromTOML
    genList
    listToAttrs
    substring
    ;
  ansiNames = [
    "black"
    "red"
    "green"
    "yellow"
    "blue"
    "magenta"
    "cyan"
    "white"
  ];
in
{
  hexToRgb = color: {
    r = (fromTOML "value = 0x${substring 1 2 color}").value;
    g = (fromTOML "value = 0x${substring 3 2 color}").value;
    b = (fromTOML "value = 0x${substring 5 2 color}").value;
  };

  mkThemePalette =
    {
      dark,
      roles,
      ansi,
    }:
    let
      ansiGroup =
        offset:
        listToAttrs (
          genList (i: {
            name = elemAt ansiNames i;
            value = elemAt ansi (offset + i);
          }) 8
        );
      shell = {
        primary = roles.accent;
        on_primary = roles.background;
        secondary = roles.secondary;
        on_secondary = roles.background;
        tertiary = roles.tertiary;
        on_tertiary = roles.background;
        error = roles.error;
        on_error = roles.background;
        surface = roles.background;
        on_surface = roles.foreground;
        surface_container = roles.panel;
        on_surface_variant = roles.muted;
        outline_variant = roles.border;
        shadow = roles.outer;
        hover = roles.surface;
        on_hover = roles.foreground;
        terminal = {
          background = roles.background;
          foreground = roles.foreground;
          cursor = roles.cursor;
          cursor_text = roles.cursorText;
          selection_bg = roles.selection;
          selection_fg = roles.selectionText;
          normal = ansiGroup 0;
          bright = ansiGroup 8;
        };
      };
    in
    {
      inherit dark roles ansi;
      noctalia = {
        version = 2;
        dark = shell;
        light = shell;
      };
    };
}
