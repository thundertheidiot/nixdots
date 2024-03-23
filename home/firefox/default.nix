{
  config,
  pkgs,
  ...
}:
with config; {
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
  };
}
