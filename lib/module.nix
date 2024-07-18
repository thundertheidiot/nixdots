{...}: rec {
  inherit (builtins) hasAttr;

  getSystem = m: if hasAttr "system" m then m.system else {};
  getHome = m: if hasAttr "home" m then m.home else {};

  getSystems = list: map (m: getSystem m) list;
  getHomes = list: map (m: getHome m) list;
}
