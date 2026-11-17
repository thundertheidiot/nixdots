# You are now in home-manager land
{ ... }: {
  imports = [
    ./ai
    ./base.nix
    ./browser
    ./cleanup.nix
    ./languages
    ./media.nix
    ./navidrome-sync.nix
    ./programs
    ./shell.nix
    ./theme
  ];
}
