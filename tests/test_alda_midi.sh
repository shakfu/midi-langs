#!/bin/bash
# Test script for alda-midi

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="$PROJECT_ROOT/build"

ALDA_MIDI="$BUILD_DIR/alda_midi"

# Check if binary exists
if [ ! -x "$ALDA_MIDI" ]; then
    echo "Error: alda_midi not found at $ALDA_MIDI"
    echo "Run 'make' first to build the project"
    exit 1
fi

echo "=== Testing alda_midi ==="

# Test 1: Help flag
echo "Test 1: Help flag..."
"$ALDA_MIDI" --help > /dev/null 2>&1
echo "  PASS: Help flag works"

# Test 2: Parse basic.alda with --no-sleep (no MIDI output needed)
echo "Test 2: Parse and interpret basic.alda..."
"$ALDA_MIDI" --no-sleep "$SCRIPT_DIR/alda/basic.alda" 2>/dev/null || {
    echo "  FAIL: Could not parse basic.alda"
    exit 1
}
echo "  PASS: basic.alda parsed and interpreted"

# Test 3: Test with verbose flag
echo "Test 3: Verbose mode..."
OUTPUT=$("$ALDA_MIDI" -v --no-sleep "$SCRIPT_DIR/alda/basic.alda" 2>&1)
if echo "$OUTPUT" | grep -q "Scheduled"; then
    echo "  PASS: Verbose mode shows scheduled events"
else
    echo "  FAIL: Verbose mode not working"
    exit 1
fi

# Test 4: Parse chords.alda
echo "Test 4: Parse chords.alda..."
cat > /tmp/chords.alda << 'EOF'
# Chord test
piano:
  c4/e/g c/e/g/b
EOF
"$ALDA_MIDI" --no-sleep /tmp/chords.alda 2>/dev/null
echo "  PASS: Chords parsed"

# Test 5: Parse voices.alda
echo "Test 5: Parse voices.alda..."
cat > /tmp/voices.alda << 'EOF'
# Voice test
piano:
  V1: c4 d e f
  V2: e4 f g a
  V0: c1
EOF
"$ALDA_MIDI" --no-sleep /tmp/voices.alda 2>/dev/null
echo "  PASS: Voices parsed"

# Test 6: Parse dynamics.alda
echo "Test 6: Parse dynamics and attributes..."
cat > /tmp/dynamics.alda << 'EOF'
# Dynamics test
piano:
  (mf)
  c4 d e f
  (tempo 140)
  g a b > c
EOF
"$ALDA_MIDI" --no-sleep /tmp/dynamics.alda 2>/dev/null
echo "  PASS: Dynamics and attributes parsed"

# Cleanup
rm -f /tmp/chords.alda /tmp/voices.alda /tmp/dynamics.alda

echo ""
echo "=== All alda_midi tests passed ==="
