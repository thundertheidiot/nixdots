{ variant }:
let
  inherit (builtins)
    fromJSON
    readFile
    mapAttrs
    hasAttr
    ;
  data = fromJSON (readFile ../madoromi/palette.json);
  selected =
    if hasAttr variant data.flavours then
      data.flavours.${variant}
    else
      throw "unknown madoromi theme variant ${variant}";
  colors = mapAttrs (_: color: color.hex) selected.colours;
  roleMap = data.roles // {
    background = "kami";
    foreground = "sumi";
    panel = "hyoshi";
    outer = "chitsu";
    surface = "nijimi0";
    backdrop = "nijimi0";
    hover = "nijimi1";
    border = "nijimi1";
    muted = "usuzumi0";
    subtle = "kasure2";
    cursorText = "kami";
    selection = "nijimi2";
    selectionText = "sumi";
    secondary = "kikyo";
    tertiary = "beni";
  };
in
{
  inherit (selected) dark ansi;
  roles = mapAttrs (_: key: colors.${key}) roleMap;
}
