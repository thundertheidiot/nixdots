{
  writeShellApplication,
  tofi,
  grim,
  slurp,
  swappy,
  hyprland,
  wl-clipboard,
  picturesDir ? "$HOME/Pictures",
  ...
}:
writeShellApplication {
  name = "wl_screenshot";

  excludeShellChecks = ["SC1091"];

  runtimeInputs = [
    tofi
    grim
    slurp
    swappy
    hyprland
    wl-clipboard
  ];

  # TODO rewrite good, maybe janet or haskell? also useless dep on hyprland when sway or etc is introduced
  text = ''
    #!/bin/sh
    . "$XDG_CONFIG_HOME/user-dirs.dirs"

    date="$(date +%Y-%m-%d_%H-%M-%S)"
    dir="${picturesDir}/screenshots"

    [ "$1" = "region" ] && {
      grim -g "$(slurp)" - | wl-copy -t image/png
      exit 0
    }

    [ ! -d "$dir" ] && mkdir --parents "$dir"

    choice=$(printf "region\nregion save\nregion with annotation\noutput\noutput save\noutput with annotation" | tofi)

    [ "$choice" = "region" ] && grim -g "$(slurp)" - | wl-copy -t image/png
    [ "$choice" = "region save" ] && grim -g "$(slurp)" -t png "$dir/$date.png"
    [ "$choice" = "region with annotation" ] && grim -g "$(slurp)" - | swappy -f -

    [ "$choice" = "output" ] && {
      grim -o "$(hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | tofi)" - | wl-copy -t image/png
    }

    [ "$choice" = "output save" ] && {
      grim -o "$(hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | tofi)" -t png "$dir/$date.png"
    }

    [ "$choice" = "output with annotation" ] && {
      grim -o "$(hyprctl monitors -j | jq '.[] | .name' | sed 's/"//g' | tofi)" - | swappy -f -
    }
  '';
}
# screenshot = pkgs.writeShellApplication {
#   name = "screenshot";
#   runtimeInputs = with pkgs; [tofi grim slurp swappy hyprland wl-clipboard];
#   text = ''
#     dir = "${config.xdg.userDirs.pictures}/screenshots"
#     [ ! -d "$dir" ] && mkdir -p "$dir"
#     region=0
#     output=0
#     save=0
#     clipboard=0
#     annotate=0
#     file=""
#     exit_parent() {
#       echo "Error: $ERROR" > /dev/tty
#       exit 1
#     }
#     while getopts 'roascf:' OPTION; do
#       case "$OPTION" in
#         r) region=1 ;;
#         o) output=1 ;;
#         s) save=1 ;;
#         c) clipboard=1 ;;
#         a) annotate=1 ;;
#         f) file=$OPTARG ;;
#       esac
#     done
#     [ $region & $output ] && { ERROR="Cannot specify both region and output at the same time" trap exit_parent EXIT; }
#     [ $save & $clipboard ] && { ERROR="Cannot specify both save and clipboard at the same time" trap exit_parent EXIT; }
#     if [ $region & $clipboard ]; then
#       grim -g "$(slurp)"
#     elif
#     fi
#   '';
# };

