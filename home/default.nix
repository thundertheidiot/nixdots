{ pkgs, localconfig, inputs, ... }: {
  home = {
    username = localconfig.username;
    homeDirectory = localconfig.homeDirectory;
    stateVersion = localconfig.homeStateVersion;
  };

  scheme = "${inputs.tt-schemes}/base16/shades-of-purple.yaml";

  imports = [
    inputs.base16.homeManagerModule
    ./env.nix
    ./basic.nix
    (import ./emacs { inherit pkgs inputs; })
  ]
  ++ (if localconfig.install.firefox then [ ./firefox.nix ] else [])
  ++ (if localconfig.install.hyprland then [ (import ./hyprland.nix { inherit pkgs inputs; } ) ] else []);

  home.packages = with pkgs; [
    fish
    alacritty
    fd
    ripgrep
  ];
}
