#!/usr/bin/env bash
# Shell hook for Modsefa development environment

echo -e "\e[1;33mᛗ\e[0m Modsefa Development Environment"
echo ""

# Create keys directory if it doesn't exist
mkdir -p keys

# Source the keylist script to set up environment variables and display info
if [ -f "./scripts/keylist.sh" ]; then
  source ./scripts/keylist.sh
else
  echo "⚠️  Warning: keylist.sh not found"
  echo ""
fi

# Function to generate keys and immediately export the address
keygen() {
  ./scripts/keygen.sh "$@"
  local exit_code=$?
  
  # If successful and we have a simple key name, export the address
  if [ $exit_code -eq 0 ] && [ $# -eq 1 ]; then
    local key_name="$1"
    local address=$(./scripts/key-to-address.sh "$key_name" 2>/dev/null || echo "")
    if [ -n "$address" ]; then
      local var_name=$(echo "$key_name" | tr '[:lower:]-' '[:upper:]_')
      export "${var_name}_ADDRESS=$address"
      echo "✓ Exported: ${var_name}_ADDRESS"
    fi
  fi
  
  return $exit_code
}

# Function to delete keys and unset the environment variable
keydelete() {
  for key_name in "$@"; do
    ./scripts/keydelete.sh "$key_name"
    
    # Unset the environment variable
    local var_name=$(echo "$key_name" | tr '[:lower:]-' '[:upper:]_')
    unset "${var_name}_ADDRESS"
  done
}

# Function to get the address for a key (just calls the script)
key-to-address() {
  source ./scripts/key-to-address.sh "$@"
}

# Function to list keys (just calls the script)
keylist() {
  source ./scripts/keylist.sh "$@"
}

# Function to display help/quick commands
help() {
  echo "→ Quick Commands:"
  echo ""
  echo "   keygen <name>         - Generate keys"
  echo "   keydelete <name>      - Delete keys"
  echo "   key-to-address <name> - Get address"
  echo "   keylist               - List all keys and addresses"
  echo "   help                  - Show this help message"
}

help