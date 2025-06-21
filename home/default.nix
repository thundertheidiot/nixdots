# You are now in home-manager land
{...}: {
  imports = [
    ./emacs
    ./languages
    ./browser
    ./media.nix
    ./cleanup.nix
    ./shell.nix
    ./base.nix
  ];
}
