{...}: let
  inherit (builtins) readFileType pathExists;
in {
  getHostConfig = host: let
    file = ../hosts/${host}.nix;
    dir = ../hosts/${host};
  in
    if (pathExists dir) && (readFileType dir) == "directory"
    then import dir
    else if (pathExists file) && (readFileType file) == "regular"
    then import file 
    else throw "No ${file} or ${dir}/default.nix";
}
