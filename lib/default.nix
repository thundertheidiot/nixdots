{ lib, ... }: {
  colors = config:
    with config.scheme.withHashtag; {
      background = base00;
      foreground = base07;
      main = base04;
      inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
    };
  
  colorsNoHash = config:
    with config.scheme; {
      background = base00;
      foreground = base07;
      main = base04;
      inherit base00 base01 base02 base03 base04 base05 base06 base07 base08 base09 base10 base11 base12 base13 base14 base15;
    };
  getSystems = list: builtins.map (module: module.system) list;
  getHomes = list: builtins.map (module: module.home) list;
}
