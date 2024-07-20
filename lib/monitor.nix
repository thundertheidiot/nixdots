{...}:
with builtins; rec {
  mkMon = {
    name,
    width ? "1920",
    height ? "1080",
    refresh ? "60",
    x ? "0",
    y ? "0",
    scale ? "1",
    hyprlandExtra ? "",
    hyprlandExclude ? false,
    edid ? false,
    customModes ? false,
    ...
  }: (mapAttrs (
      name: value:
        if
          ( # nonstringified attrs
            name
            != "edid"
            && name != "hyprlandExclude"
            && name != "customModes"
          )
        then toString value
        else value
    ) {
      inherit
        name
        width
        height
        refresh
        x
        y
        hyprlandExtra
        hyprlandExclude
        edid
        customModes
        ;
    });

  mkMonitors = list: (map (
      mon:
        if isAttrs mon
        then mkMon mon
        else throw "Monitor ${mon} is not an attribute set."
    )
    list);
}
