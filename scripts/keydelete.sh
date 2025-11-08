#!/usr/bin/env bash
# Delete Cardano key pairs (no confirmation)

if [ $# -eq 0 ]; then
    echo "Usage: $0 <key-name> [<key-name> ...]"
    echo "Example: $0 alice bob"
    exit 1
fi

for key_name in "$@"; do
    VKEY_FILE="keys/${key_name}.vkey"
    SKEY_FILE="keys/${key_name}.skey"
    
    if [ ! -f "$VKEY_FILE" ] && [ ! -f "$SKEY_FILE" ]; then
        echo "⚠️  Key pair '$key_name' not found"
        continue
    fi
    
    # Delete the files
    [ -f "$VKEY_FILE" ] && rm "$VKEY_FILE" && echo "✓ Deleted $VKEY_FILE"
    [ -f "$SKEY_FILE" ] && rm "$SKEY_FILE" && echo "✓ Deleted $SKEY_FILE"
    
    # Unset the environment variable
    var_name=$(echo "$key_name" | tr '[:lower:]-' '[:upper:]_')
    if [ -n "${!var_name}_ADDRESS" ]; then
        unset "${var_name}_ADDRESS"
        echo "✓ Unset ${var_name}_ADDRESS"
    fi
done