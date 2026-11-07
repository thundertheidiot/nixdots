{ source, variant }:
let
  inherit (builtins)
    fromJSON
    readFile
    mapAttrs
    map
    hasAttr
    ;
  data = fromJSON (readFile "${source}/palette.json");
  selected =
    if hasAttr variant data && variant != "version" then
      data.${variant}
    else
      throw "unknown catppuccin theme variant ${variant}";
  colors = mapAttrs (_: color: color.hex) selected.colors;
  roleMap = {
    background = "base";
    foreground = "text";
    panel = "mantle";
    outer = "crust";
    surface = "surface0";
    backdrop = "surface1";
    hover = "surface1";
    border = "surface0";
    muted = "subtext0";
    subtle = "overlay0";
    accent = "mauve";
    cursor = "rosewater";
    cursorText = "base";
    selection = "surface2";
    selectionText = "text";
    error = "red";
    warning = "yellow";
    info = "blue";
    success = "green";
    keyword = "mauve";
    string = "green";
    function = "blue";
    type = "yellow";
    number = "peach";
    comment = "overlay0";
    link = "sapphire";
    secondary = "blue";
    tertiary = "red";
  };
in
{
  inherit (selected) dark;
  roles = mapAttrs (_: key: colors.${key}) roleMap;
  ansi = map (key: colors.${key}) [
    "base"
    "red"
    "green"
    "yellow"
    "blue"
    "pink"
    "teal"
    "subtext1"
    "surface2"
    "red"
    "green"
    "yellow"
    "blue"
    "pink"
    "teal"
    "subtext0"
  ];
}
