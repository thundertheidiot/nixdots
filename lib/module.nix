{
  getSystems = list: builtins.map (module: module.system) list;
  getHomes = list: builtins.map (module: module.home) list;
}
