#!/usr/bin/env nix-shell
#! nix-shell -i bash -p bash openssl sops jq git coreutils diffutils nix
# shellcheck shell=bash

set -euo pipefail

if [[ ${1-} == --help && $# == 1 ]]; then
  printf '%s\n' \
    'Usage: local-certs (or certs/create.sh)' \
    'Run only against a trusted admin checkout; Nix evaluates its configuration.' \
    'Regenerates the CA and server certificate using server2 domains and certs/default.nix aliases.' \
    'Requires an admin SOPS identity to verify the encrypted leaf key.' \
    'Discards the new CA private key. Does not remove the old encrypted rootCA.key.' \
    'The new CA must be trusted by clients before the new leaf is deployed.'
  exit 0
fi
if (( $# != 0 )); then
  printf 'Usage: local-certs (or certs/create.sh); see --help\n' >&2
  exit 1
fi

# The checkout script also works outside the repo; the packaged copy uses cwd.
repo_root=$(git -C "$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")" \
  rev-parse --show-toplevel 2>/dev/null || git rev-parse --show-toplevel)
cd -- "$repo_root"
[[ -f flake.nix && -f certs/default.nix && -f .sops.yaml ]] || {
  printf 'Run local-certs in the trusted nixdots checkout.\n' >&2
  exit 1
}

umask 077
ulimit -c 0
# Ignore TMPDIR so plaintext keys cannot accidentally land in the repo/store.
work=$(mktemp -d /tmp/local-certs.XXXXXXXX)
stage=
trap 'rm -rf -- "$work"; if [[ -n "$stage" ]]; then rm -rf -- "$stage"; fi' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP

cert_days=825
ca_days=1825
nix eval --json --no-write-lock-file \
  .#nixosConfigurations.server2.config.server.domains > "$work/host-domains.json"
nix eval --json --file ./certs/default.nix domains > "$work/aliases.json"
jq -s -e '
  add + ["*.home"] | unique |
  if length > 0 and all(.[]; type == "string" and
    test("^(\\*\\.)?[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?(\\.[A-Za-z0-9]([A-Za-z0-9-]*[A-Za-z0-9])?)*$"))
  then . else error("Invalid DNS name in certificate domains") end
' "$work/host-domains.json" "$work/aliases.json" > "$work/domains.json"
san_dns=$(jq -r 'map("DNS:" + .) | join(",")' "$work/domains.json")

printf 'Regenerating CA and leaf for %s\n' "$san_dns"
openssl genrsa -out "$work/rootCA.key" 4096
openssl req -x509 -new -config /dev/null -key "$work/rootCA.key" \
  -sha256 -days "$ca_days" -out "$work/rootCA.pem" -subj '/CN=Local Dev CA' \
  -addext 'basicConstraints=critical,CA:TRUE' \
  -addext 'keyUsage=critical,keyCertSign,cRLSign' \
  -addext 'subjectKeyIdentifier=hash' \
  -addext 'authorityKeyIdentifier=keyid:always'

openssl genrsa -out "$work/local.key" 2048
openssl req -new -config /dev/null -key "$work/local.key" \
  -out "$work/local.csr" -subj '/CN=*.home'
printf '%s\n' \
  'basicConstraints=critical,CA:FALSE' \
  'keyUsage=critical,digitalSignature,keyEncipherment' \
  'extendedKeyUsage=serverAuth' \
  'subjectKeyIdentifier=hash' \
  'authorityKeyIdentifier=keyid,issuer' \
  "subjectAltName=$san_dns" > "$work/san.ext"
openssl x509 -req -in "$work/local.csr" -CA "$work/rootCA.pem" \
  -CAkey "$work/rootCA.key" -set_serial "0x$(openssl rand -hex 16)" \
  -out "$work/local.crt" -days "$cert_days" -sha256 -extfile "$work/san.ext"

for name in rootCA local; do
  cert="$work/local.crt"
  days=$cert_days
  if [[ $name == rootCA ]]; then
    cert="$work/rootCA.pem"
    days=$ca_days
  fi
  openssl pkey -in "$work/$name.key" -check -noout
  openssl pkey -in "$work/$name.key" -pubout -out "$work/key.pub"
  openssl x509 -in "$cert" -pubkey -noout > "$work/cert.pub"
  cmp -- "$work/key.pub" "$work/cert.pub"
  openssl x509 -in "$cert" -noout -checkend "$(((days - 1) * 86400))"
done
openssl verify -x509_strict -check_ss_sig -no-CApath -no-CAstore \
  -CAfile "$work/rootCA.pem" "$work/rootCA.pem"
openssl verify -x509_strict -no-CApath -no-CAstore -purpose sslserver \
  -CAfile "$work/rootCA.pem" "$work/local.crt"
actual_san=$(openssl x509 -in "$work/local.crt" -noout -ext subjectAltName)
actual_san=${actual_san#*$'\n'}
actual_san=${actual_san//[[:space:]]/}
[[ $actual_san == "$san_dns" ]] || {
  printf 'Certificate SANs do not match the requested domains.\n' >&2
  exit 1
}
rm -- "$work/rootCA.key"

sops --encrypt --config "$repo_root/.sops.yaml" \
  --filename-override "$repo_root/certs/local.key" \
  --input-type binary --output-type binary \
  --output "$work/local.key.sops" "$work/local.key"
sops --decrypt --input-type binary --output-type binary "$work/local.key.sops" \
  | cmp -- "$work/local.key" -
rm -- "$work/local.key"

# Only ciphertext and public certificates enter the checkout, after validation.
# Stage on the destination filesystem so each final rename is atomic.
stage=$(mktemp -d "$repo_root/certs/.local-certs.XXXXXXXX")
install -m 644 -- "$work/local.key.sops" "$stage/local.key"
install -m 644 -- "$work/rootCA.pem" "$stage/rootCA.pem"
install -m 644 -- "$work/local.crt" "$stage/local.crt"
for file in local.key rootCA.pem local.crt; do
  mv -fT -- "$stage/$file" "$repo_root/certs/$file"
done
printf '%s\n' \
  'Updated certs/local.key, certs/rootCA.pem and certs/local.crt.' \
  'New CA private key discarded; existing certs/rootCA.key left untouched.' \
  'Trust the new rootCA.pem on clients before deploying the new leaf.'
