{
  writers,
  tofi,
}:
writers.writeHaskellBin "screenshot" {
  libraries = [
    tofi
  ];
}
(builtins.readFile
  ./main.hs)
