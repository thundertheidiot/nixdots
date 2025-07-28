#!/usr/bin/env nix-shell
#! nix-shell -i bash -p bash openssl sops

set -euo pipefail

# Configuration
CERT_DAYS=825
CA_DAYS=1825
COMMON_NAME="*.home"
SAN_DNS="DNS:auth.home,DNS:n8n.home,DNS:*.home"

# Generate private key for root CA
openssl genrsa -out rootCA.key 4096

# Generate self-signed root certificate
openssl req -x509 -new -nodes -key rootCA.key -sha256 -days "$CA_DAYS" \
  -out rootCA.pem \
  -subj "/CN=Local Dev CA"

# Generate private key for local cert
openssl genrsa -out local.key 2048

# Generate CSR (Certificate Signing Request)
openssl req -new -key local.key -out local.csr \
  -subj "/CN=${COMMON_NAME}"

# Create config for SANs (SubjectAltNames)
cat > san.ext <<EOF
authorityKeyIdentifier=keyid,issuer
basicConstraints=CA:FALSE
subjectAltName=${SAN_DNS}
EOF

# Sign the CSR with the Root CA
openssl x509 -req -in local.csr -CA rootCA.pem -CAkey rootCA.key \
  -CAcreateserial -out local.crt -days "$CERT_DAYS" -sha256 -extfile san.ext

echo "=============="
echo "Keys generated"
echo "=============="

# Encrypt private keys in-place with SOPS
mv rootCA.key rootCA.key.plain
sops -e --output rootCA.key --input-type binary --output-type binary rootCA.key.plain
rm rootCA.key.plain

mv local.key local.key.plain
sops -e --output local.key --input-type binary --output-type binary local.key.plain
rm local.key.plain

echo "=============="
echo "Keys encrypted"
echo "=============="

# Cleanup
rm -f rootCA.srl local.csr san.ext
