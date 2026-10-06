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
EXAMPLES_DIR="$PROJECT_DIR/projects/mhs-midi/examples"

if [ ! -f "$MHS_MIDI" ]; then
    echo "Error: mhs-midi not found at $MHS_MIDI"
    exit 1
fi

echo "Running mhs-midi unit tests..."
echo "Binary: $MHS_MIDI"
echo ""

# Run the Music.hs test module
OUTPUT=$(env MHSDIR="$MHS_DIR" "$MHS_MIDI" -r -C -i"$MIDI_LIB" TestMusic 2>&1)
EXIT_CODE=$?

echo "$OUTPUT"

# Check for Music test failures
if ! echo "$OUTPUT" | grep -q "ALL TESTS PASSED"; then
    echo ""
    echo "Music test suite failed!"
    exit 1
fi

echo ""
echo "Music tests passed."
echo ""

# Run the Async test module
echo "Running Async tests..."
ASYNC_OUTPUT=$(env MHSDIR="$MHS_DIR" "$MHS_MIDI" -r -C -i"$MIDI_LIB" -i"$EXAMPLES_DIR" AsyncTest 2>&1)
ASYNC_EXIT=$?

echo "$ASYNC_OUTPUT"

# Check for Async test failures
if ! echo "$ASYNC_OUTPUT" | grep -q "All tests passed"; then
    echo ""
    echo "Async test suite failed!"
    exit 1
fi

# pitchBendCents: 0 cents must be centre. aseqdump prints bends relative to centre.
# Real backend only: the null backend never reaches aseqdump.
echo ""
if [ -c /dev/snd/seq ] && command -v aseqdump > /dev/null; then
    echo "Running pitchBendCents test..."
    DUMP_LOG=$(mktemp)
    stdbuf -oL aseqdump > "$DUMP_LOG" 2>&1 &
    DUMP_PID=$!
    for _ in $(seq 1 50); do
        aconnect -o 2>/dev/null | grep -q "'aseqdump'" && break
        sleep 0.1
    done
    BEND_OUTPUT=$(env -u MIDI_LANGS_BACKEND MHSDIR="$MHS_DIR" "$MHS_MIDI" -r -C -i"$MIDI_LIB" -i"$SCRIPT_DIR/mhs" PitchBendTest 2>&1)
    sleep 0.2
    kill "$DUMP_PID"; wait "$DUMP_PID" 2>/dev/null || true
    BENDS=$(grep "Pitch bend" "$DUMP_LOG" | sed -E 's/.*value //' | tr '\n' ' ')
    rm -f "$DUMP_LOG"
    if [ "$BENDS" != "0 4096 -8192 " ]; then
        echo "$BEND_OUTPUT"
        echo "pitchBendCents test failed: expected '0 4096 -8192 ', got '$BENDS'"
        exit 1
    fi
    echo "pitchBendCents test passed."
else
    echo "SKIP: pitchBendCents test (no ALSA sequencer or aseqdump)"
fi

echo ""
echo "All test suites passed!"
exit 0
