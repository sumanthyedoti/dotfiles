#!/bin/bash

chmod +x "$0"

temperature=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits)
utilization=$(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits)

memory_used=$(nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits)
memory_total=$(nvidia-smi --query-gpu=memory.total --format=csv,noheader,nounits)
memory_percentage=$((memory_used * 100 / memory_total))

echo "🖼️  ${temperature}°C : 💪 ${utilization}% : 🧠 ${memory_percentage}%"
