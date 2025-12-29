#!/bin/bash
# test_mhs_midi.sh - Run mhs-midi unit tests
#
# Usage: test_mhs_midi.sh <mhs-midi-binary>

set -e

MHS_MIDI="${1:-./build/mhs-midi}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"
MHS_DIR="$PROJECT_DIR/thirdparty/MicroHs"
MIDI_LIB="$PROJECT_DIR/projects/mhs-midi/lib"

if [ ! -f "$MHS_MIDI" ]; then
    echo "Error: mhs-midi not found at $MHS_MIDI"
    exit 1
fi

echo "Running mhs-midi unit tests..."
echo "Binary: $MHS_MIDI"
echo ""

# Run the test module (use -r flag for direct execution)
OUTPUT=$(env MHSDIR="$MHS_DIR" "$MHS_MIDI" -r -C -i"$MIDI_LIB" TestMusic 2>&1)
EXIT_CODE=$?

echo "$OUTPUT"

# Check for test failures
if echo "$OUTPUT" | grep -q "ALL TESTS PASSED"; then
    echo ""
    echo "Test suite passed!"
    exit 0
else
    echo ""
    echo "Test suite failed!"
    exit 1
fi
