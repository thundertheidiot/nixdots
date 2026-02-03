# You are now in home-manager land
{...}: {
  imports = [
    ./languages
    ./programs
    ./browser
    ./media.nix
    ./cleanup.nix
    ./shell.nix
    ./base.nix
    ./theme.nix
  ];
}
