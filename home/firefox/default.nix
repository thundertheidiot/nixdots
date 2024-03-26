{
  config,
  pkgs,
  lib,
  localconfig,
  ...
}: {
  config = lib.mkIf (localconfig.install.firefox) (with config; {
    programs.firefox = {
      enable = true;
      package = pkgs.firefox;
    };
  });
}
