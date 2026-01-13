#!/bin/bash
# Cross-platform clipboard copy script
# Usage: command | copyc

# Read from stdin
input=$(cat)

# Detect OS and use appropriate clipboard command
if [[ "$OSTYPE" == "darwin"* ]]; then
	# macOS
	echo -n "$input" | pbcopy
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
	# Linux - try different clipboard tools
	if command -v xclip &>/dev/null; then
		echo -n "$input" | xclip -selection clipboard
	elif command -v xsel &>/dev/null; then
		echo -n "$input" | xsel --clipboard --input
	elif command -v wl-copy &>/dev/null; then
		# Wayland
		echo -n "$input" | wl-copy
	else
		echo "Error: No clipboard tool found. Please install xclip, xsel, or wl-clipboard" >&2
		exit 1
	fi
else
	echo "Error: Unsupported operating system: $OSTYPE" >&2
	exit 1
fi

echo "✓ Copied to clipboard"
