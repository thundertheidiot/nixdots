{...}: let
  inherit (builtins) readFileType pathExists;
in {
  getHostConfig = host: let
    file = ../hosts/${host}.nix;
    dir = ../hosts/${host};
  in
    if (pathExists dir) && (readFileType dir) == "directory"
    then import dir
    else if (pathExists file) && (readFileType ../hosts/${host}.nix) == "regular"
    then import ../hosts/${host}.nix
    else throw "No hosts/${host}.nix or hosts/${host}/default.nix";
}
