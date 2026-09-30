#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/../.."

private_key=$(sops -d --extract '["phone"]' sops/wireguard/private-keys.json)
preshared_key=$(sops -d sops/wireguard/preshared-key)
vps_pubkey=$(jq -r .vps sops/wireguard/public-keys.json)

cat <<CONF | qrencode -t ansiutf8
[Interface]
Address = 10.100.0.6/24
DNS = 10.100.0.1
PrivateKey = $private_key

[Peer]
PublicKey = $vps_pubkey
PresharedKey = $preshared_key
Endpoint = 95.216.151.56:51820
PersistentKeepalive = 25
AllowedIPs = 10.100.0.0/24
CONF
