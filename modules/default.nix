[
  # "libraries"
  # Probably required by the below options, include these
  ./home.nix # most likely conflicts with your home-manager setup, read flake.nix for some pointers
  # ./config

  # more abstract configuration
  # These may be useful
  ./impermanence.nix
  ./monitor.nix
  ./gpu.nix

  # specific configuration
  # These are probably less useful
  ./base.nix
  ./shell.nix
  ./ssh.nix
  ./programs
  ./vm.nix
  ./tv
  ./emacs
  ./xorg.nix
  ./firefox.nix
  ./browser
  ./workstation
  ./cleanup.nix
  ./keyboard
  ./gaming

  # TODO: reorganize modules/workstation
  (import ./old-workstation).system
  ({mlib, ...}: mlib.homeModule (import ./old-workstation).home)
]
