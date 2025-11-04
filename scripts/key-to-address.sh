#!/usr/bin/env bash
# Derive Cardano address from verification key using network from config.json

if [ $# -lt 1 ]; then
    echo "Usage: $0 <key-name> [config-file]"
    echo "       $0 <path-to-vkey-file> [config-file]"
    echo ""
    echo "Examples:"
    echo "  $0 alice                    # Derive address from keys/alice.vkey"
    echo "  $0 bob                      # Derive address from keys/bob.vkey"
    echo "  $0 custom/path.vkey         # Custom path"
    exit 1
fi

KEY_INPUT="$1"
CONFIG_FILE="${2:-./config.json}"

# Determine if input is a key name or a path
if [[ "$KEY_INPUT" == *.vkey ]]; then
    # It's a path to a vkey file
    VKEY_FILE="$KEY_INPUT"
else
    # It's a key name - look in keys/ directory
    VKEY_FILE="keys/${KEY_INPUT}.vkey"
fi

# Check if the file exists
if [ ! -f "$VKEY_FILE" ]; then
    echo "Error: Verification key file not found: $VKEY_FILE"
    exit 1
fi

python3 - "$VKEY_FILE" "$CONFIG_FILE" << 'PYTHON_EOF'
import sys, json, hashlib

def bech32_encode(hrp, data):
    """Bech32 encoder - fixed version"""
    CHARSET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
    
    def bech32_polymod(values):
        GEN = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]
        chk = 1
        for v in values:
            b = chk >> 25
            chk = (chk & 0x1ffffff) << 5 ^ v
            for i in range(5):
                chk ^= GEN[i] if ((b >> i) & 1) else 0
        return chk
    
    def bech32_hrp_expand(hrp):
        return [ord(x) >> 5 for x in hrp] + [0] + [ord(x) & 31 for x in hrp]
    
    def convertbits(data, frombits, tobits, pad=True):
        acc = 0
        bits = 0
        ret = []
        maxv = (1 << tobits) - 1
        max_acc = (1 << (frombits + tobits - 1)) - 1
        for value in data:
            if value < 0 or (value >> frombits):
                return None
            acc = ((acc << frombits) | value) & max_acc
            bits += frombits
            while bits >= tobits:
                bits -= tobits
                ret.append((acc >> bits) & maxv)
        if pad:
            if bits:
                ret.append((acc << (tobits - bits)) & maxv)
        elif bits >= frombits or ((acc << (tobits - bits)) & maxv):
            return None
        return ret
    
    values = bech32_hrp_expand(hrp) + convertbits(data, 8, 5)
    polymod = bech32_polymod(values + [0, 0, 0, 0, 0, 0]) ^ 1
    checksum = [(polymod >> 5 * (5 - i)) & 31 for i in range(6)]
    combined = convertbits(data, 8, 5) + checksum
    return hrp + '1' + ''.join([CHARSET[d] for d in combined])

# Read config to determine network
try:
    with open(sys.argv[2], 'r') as f:
        config = json.load(f)
    network_id = config.get('networkId', 'testnet').lower()
except (FileNotFoundError, json.JSONDecodeError, KeyError):
    print("Warning: Could not read config.json, defaulting to testnet", file=sys.stderr)
    network_id = 'testnet'

# Read verification key
with open(sys.argv[1], 'r') as f:
    vkey_data = json.load(f)

# Extract public key bytes
vkey_hex = vkey_data['cborHex'][4:]  # Remove '5820' prefix
vkey_bytes = bytes.fromhex(vkey_hex)

# Hash the public key (Blake2b-224 for key hash)
vkey_hash = hashlib.blake2b(vkey_bytes, digest_size=28).digest()

# Create enterprise address (payment credential only, no stake)
if network_id == 'mainnet':
    network_tag = 0b0001  # mainnet enterprise address
    hrp = 'addr'
else:  # testnet, preview, preprod
    network_tag = 0b0110  # testnet enterprise address  
    hrp = 'addr_test'

# Header byte: 0110 0000 for testnet enterprise (bits: type=0110, unused=0000)
header = (network_tag << 4)
payload = bytes([header]) + vkey_hash

# Bech32 encode
address = bech32_encode(hrp, payload)
print(address)
PYTHON_EOF