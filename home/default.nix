{ pkgs, localconfig, customFiles, ... }: {
  home = {
    username = localconfig.username;
    homeDirectory = localconfig.homeDirectory;
    stateVersion = localconfig.homeStateVersion;
  };

  imports = [
    ./env.nix
    ./basic.nix
    (import ./emacs { inherit pkgs customFiles; })
  ] ++ (if localconfig.install.firefox then [ ./firefox.nix ] else []);

  home.packages = with pkgs; [
    fish
    alacritty
    fd
    ripgrep
  ];
}
