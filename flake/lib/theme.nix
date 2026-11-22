{ ... }:
let
  inherit (builtins)
    elemAt
    floor
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

  # whole-number hue, saturation and lightness percentages
  hexToHsl =
    color:
    let
      channel = offset: (fromTOML "value = 0x${substring offset 2 color}").value / 255.0;
      r = channel 1;
      g = channel 3;
      b = channel 5;
      max =
        if r > g && r > b then
          r
        else if g > b then
          g
        else
          b;
      min =
        if r < g && r < b then
          r
        else if g < b then
          g
        else
          b;
      d = max - min;
      l = (max + min) / 2;
      s = if d == 0 then 0 else d / (1 - (if l > 0.5 then 2 * l - 1 else 1 - 2 * l));
      hue =
        if d == 0 then
          0
        else if max == r then
          60 * (g - b) / d
        else if max == g then
          60 * ((b - r) / d + 2)
        else
          60 * ((r - g) / d + 4);
      round = x: floor (x + 0.5);
    in
    {
      h = round (if hue < 0 then hue + 360 else hue);
      s = round (s * 100);
      l = round (l * 100);
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
