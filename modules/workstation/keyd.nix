{
  system = {
    lib,
    config,
    pkgs,
    ...
  }:
    lib.mkIf config.workstation.keyd.enable {
      services.keyd = {
        enable = true;

        keyboards.default = {
          ids = config.workstation.keyd.ids;

          settings = {
            main = {
              capslock = "overload(meta, esc)";

              space = "overload(numpad, space)";

              # hrm
              # a = "timeout(a, 200, layer(meta))";
              a = "overload(meta, a)";
              s = "overload(alt, s)";
              d = "overload(shift, d)";
              f = "overload(control, f)";

              j = "overload(control, j)";
              k = "overload(shift, k)";
              l = "overload(meta, l)";
              ";" = "overload(meta, ;)";
            };

            numpad = {
              m = "1";
              "," = "2";
              "." = "3";
              j = "4";
              k = "5";
              l = "6";
              u = "7";
              i = "8";
              o = "9";
              rightalt = "0";

              a = "layer(meta)";
              s = "layer(alt)";
              d = "layer(shift)";
              f = "layer(control)";

              g = "toggle(normal)";
            };

            normal = {
              capslock = "overload(meta, esc)";
              rightalt = "toggle(normal)";

              a = "a";
              s = "s";
              d = "d";
              f = "f";

              j = "j";
              k = "k";
              l = "l";
              ";" = ";";

              space = "space";
            };

            shift = {
              esc = "~";
            };
            meta = {
              esc = "`";
            };
          };
        };
      };
    };

  home = {...}: {};
}
