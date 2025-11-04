#!/usr/bin/env bash
# List all Cardano key pairs and their addresses

CONFIG_FILE="${1:-./config.json}"

# Read network from config
if [ -f "$CONFIG_FILE" ]; then
  NETWORK=$(python3 -c "import json; print(json.load(open('$CONFIG_FILE'))['networkId'])" 2>/dev/null || echo "testnet")
  echo "◆ Network: $NETWORK"
else
  NETWORK="testnet"
  echo "⚠️  No config.json found, assuming testnet"
fi
echo ""

# Find all key pairs in keys/ directory
KEY_PAIRS=()
for vkey in keys/*.vkey; do
  if [ -f "$vkey" ]; then
    base_name=$(basename "${vkey%.vkey}")
    skey="keys/${base_name}.skey"
    if [ -f "$skey" ]; then
      KEY_PAIRS+=("$base_name")
    fi
  fi
done

if [ ${#KEY_PAIRS[@]} -eq 0 ]; then
  echo "⚠️  No key pairs found in keys/ directory"
  echo ""
  echo "Generate keys with:"
  echo "  keygen <name>"
  echo ""
  exit 0
fi

echo "✓ Found ${#KEY_PAIRS[@]} key pair(s) in keys/:"
echo ""

# Process each key pair and export environment variables
for key_name in "${KEY_PAIRS[@]}"; do
  address=$(./scripts/key-to-address.sh "$key_name" "$CONFIG_FILE" 2>/dev/null || echo "")
  if [ -n "$address" ]; then
    var_name=$(echo "$key_name" | tr '[:lower:]-' '[:upper:]_')
    export "${var_name}_ADDRESS=$address"
    echo "  $key_name → ${var_name}_ADDRESS"
    echo "    $address"
    echo ""
  fi
done