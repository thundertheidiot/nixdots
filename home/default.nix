# You are now in home-manager land
{...}: {
  imports = [
    ./emacs
    ./languages
    ./browser
  ];
}
