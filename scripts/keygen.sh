#!/usr/bin/env bash
# Generate Cardano Ed25519 keys

# Ensure keys directory exists
mkdir -p keys

if [ $# -eq 0 ]; then
    echo "Usage: $0 <key-name>"
    echo "       $0 <verification-key-file> <signing-key-file>"
    echo ""
    echo "Examples:"
    echo "  $0 alice                    # Creates keys/alice.vkey and keys/alice.skey"
    echo "  $0 bob                      # Creates keys/bob.vkey and keys/bob.skey"
    echo "  $0 custom/path.vkey custom/path.skey  # Custom paths"
    exit 1
fi

# Simple mode: just a name
if [ $# -eq 1 ]; then
    KEY_NAME="$1"
    VKEY_FILE="keys/${KEY_NAME}.vkey"
    SKEY_FILE="keys/${KEY_NAME}.skey"
# Advanced mode: full paths
elif [ $# -eq 2 ]; then
    VKEY_FILE="$1"
    SKEY_FILE="$2"
    # Extract key name from path for env var export
    KEY_NAME=$(basename "${VKEY_FILE%.vkey}")
else
    echo "Error: Too many arguments"
    exit 1
fi

# Check if keys already exist
if [ -f "$VKEY_FILE" ] || [ -f "$SKEY_FILE" ]; then
    echo "Error: Key files already exist:"
    [ -f "$VKEY_FILE" ] && echo "  $VKEY_FILE"
    [ -f "$SKEY_FILE" ] && echo "  $SKEY_FILE"
    echo ""
    echo "Remove them first if you want to regenerate."
    exit 1
fi

python3 - "$VKEY_FILE" "$SKEY_FILE" << 'PYTHON_EOF'
import sys, json
from cryptography.hazmat.primitives.asymmetric import ed25519
from cryptography.hazmat.primitives import serialization

private_key = ed25519.Ed25519PrivateKey.generate()
public_key = private_key.public_key()

private_bytes = private_key.private_bytes(
    encoding=serialization.Encoding.Raw,
    format=serialization.PrivateFormat.Raw,
    encryption_algorithm=serialization.NoEncryption()
)

public_bytes = public_key.public_bytes(
    encoding=serialization.Encoding.Raw,
    format=serialization.PublicFormat.Raw
)

skey = {
    "type": "PaymentSigningKeyShelley_ed25519",
    "description": "Payment Signing Key",
    "cborHex": "5820" + private_bytes.hex()
}

vkey = {
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": "5820" + public_bytes.hex()
}

with open(sys.argv[1], 'w') as f:
    json.dump(vkey, f, indent=4)

with open(sys.argv[2], 'w') as f:
    json.dump(skey, f, indent=4)

print(f"✓ Keys generated: {sys.argv[1]}, {sys.argv[2]}")
PYTHON_EOF

# Derive and export the address immediately
if [ -f "$VKEY_FILE" ]; then
    address=$(./scripts/key-to-address.sh "$KEY_NAME" 2>/dev/null || echo "")
    if [ -n "$address" ]; then
        var_name=$(echo "$KEY_NAME" | tr '[:lower:]-' '[:upper:]_')
        export "${var_name}_ADDRESS=$address"
        echo "✓ Address: $address"
    fi
fi