# You are now in home-manager land
{...}: {
  imports = [
    ./emacs
    ./languages
    ./browser
    ./cleanup.nix
    ./shell.nix
  ];
}
