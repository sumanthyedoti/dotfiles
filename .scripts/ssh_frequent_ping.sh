#!/bin/bash

# Check if both username and server IP were provided as arguments
if [[ $# -lt 2 ]]; then
    echo "Usage: $0 <username> <server_ip>"
    exit 1
fi

USERNAME="$1"
SERVER_IP="$2"
INTERVAL_SECONDS=60  # 1 minutes

# Function to send a null packet to the SSH server every specified interval
send_null_packet() {
    echo "Sending null packet to the SSH server..."
    ssh -o "ServerAliveInterval $INTERVAL_SECONDS" "$USERNAME@$SERVER_IP" "echo -n"
}

# Loop to send null packets every interval
while true; do
    send_null_packet
    sleep "$INTERVAL_SECONDS"
done

