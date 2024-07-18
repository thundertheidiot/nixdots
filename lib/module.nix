{...}: rec {
  inherit (builtins) hasAttr;

  getSystem = m: m.system or {};
  getHome = m: m.home or {};

  getSystems = list: map (m: getSystem m) list;
  getHomes = list: map (m: getHome m) list;
}
