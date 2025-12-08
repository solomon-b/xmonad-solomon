#!/usr/bin/env bash

# Get ACPI battery and adapter status
status=$(acpi -b -a 2>/dev/null)

if [[ -z "$status" ]]; then
    status="No ACPI info available"
fi

# Display via dmenu (readonly, just press Escape to close)
echo "$status" | dmenu -l 5 -p "Power:"

