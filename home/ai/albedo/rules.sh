# PreToolUse command hook (see hooks.json): reads the hook payload from
# stdin, exit 2 denies the tool call with stderr as the reason.
# @HOME@ is substituted with the home directory at build time.
home="@HOME@"
org="$home/Documents/org"

payload=$(</dev/stdin)
# expand ~ and $HOME spellings to the absolute needle
payload=${payload//'~/'/"$home"/}
payload=${payload//'$HOME'/"$home"}

deny() {
  echo "access to ~/Documents/org is denied" >&2
  exit 2
}

# the org path, followed by end, /, a closing quote, or a space
case $payload in
  *"$org"|*"$org"/*|*"$org"'"'*|*"$org"' '*) deny ;;
esac

# a bare relative Documents/org reference resolves under the session cwd
cwd=${payload#*'"cwd":"'}
cwd=${cwd#*'"cwd": "'}
cwd=${cwd%%'"'*}
case $cwd in
  "$home"|"$home"/*)
    case $payload in
      *"Documents/org"|*"Documents/org"/*|*"Documents/org"'"'*|*"Documents/org"' '*) deny ;;
    esac
    ;;
esac
