{ pkgs, localconfig, ... }: {
  home = {
    username = localconfig.username;
    homeDirectory = localconfig.homeDirectory;
    stateVersion = localconfig.homeStateVersion;
  };

  imports = [
    ./env.nix
    ./basic.nix
    ./emacs
  ] ++ (if localconfig.install.firefox then [ ./firefox.nix ] else []);

  home.packages = with pkgs; [
    fish
    alacritty
    emacs
    fd
    ripgrep
  ];
}
