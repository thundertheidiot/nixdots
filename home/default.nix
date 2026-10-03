# You are now in home-manager land
{ ... }: {
  imports = [
    ./base.nix
    ./browser
    ./cleanup.nix
    ./languages
    ./media.nix
    ./opencode
    ./programs
    ./shell.nix
    ./theme.nix
  ];
}
