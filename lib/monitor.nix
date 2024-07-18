{...}: with builtins; rec {
  mkMon = {
    name,
      width ? "1920",
      height ? "1080",
      refresh ? "60",
      x ? "0",
      y ? "0",
      scale ? "1",
      hyprlandExtra ? "",
      edid ? false,
  }: (mapAttrs (name: value:
    if (name != edid) then
      toString value
    else
      value
  ) {
        inherit
          name
          width
          height
          refresh
          x
          y
          hyprlandExtra
          edid;
      });

  mkMonitors = list: (map (mon:
    if isAttrs mon then
      mkMon mon
    else
      throw "Monitor ${mon} is not an attribute set."
  ) list);
}
