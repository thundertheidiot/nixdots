{
  system = {
    config,
    mlib,
    ...
  }: {
    boot.kernelParams = builtins.map (m: let
      mon = mlib.getMon m;
      s = s: builtins.toString s;
    in
      with mon; "video=${name}:${s width}x${s height}-32@${s refresh}")
    config.monitors;
  };

  home = {...}: {};
}
