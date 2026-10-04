#!/bin/bash
# Each binding lists output ports as "client: port".
# aseqdump provides an ALSA client named "aseqdump" with a port named "aseqdump".
#
# Usage: test_port_labels.sh <build-dir>

set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="${1:-$PROJECT_ROOT/build}"
LABEL="aseqdump: aseqdump"

if [ ! -c /dev/snd/seq ] || ! command -v aseqdump > /dev/null; then
    echo "SKIP: no ALSA sequencer or aseqdump"
    exit 0
fi

aseqdump > /dev/null 2>&1 &
DUMP_PID=$!
TMP=$(mktemp -d)
trap 'kill $DUMP_PID 2>/dev/null; rm -rf "$TMP"' EXIT

for _ in $(seq 1 50); do
    aconnect -o 2>/dev/null | grep -q "'aseqdump'" && break
    sleep 0.1
done

FAIL=0
check() {
    local name="$1" output="$2"
    if echo "$output" | grep -qF "$LABEL"; then
        echo "  PASS: $name"
    else
        echo "  FAIL: $name: '$LABEL' not in output:"
        echo "$output" | sed 's/^/    /'
        FAIL=1
    fi
}

run() {
    local name="$1" bin="$BUILD_DIR/$2" input="$3"
    shift 3
    if [ ! -x "$bin" ]; then
        echo "  SKIP: $name (not built)"
        return
    fi
    check "$name" "$(echo "$input" | timeout 60 "$bin" "$@" 2>&1)"
}

echo "=== Port labels ==="
run alda   alda_midi  "list"
run forth  forth_midi "midi-output-list"
run joy    joy_midi   "midi-list"
run lua    lua_midi   'for _,p in ipairs(midi.list_ports()) do print(p[2]) end'
run s7     s7_midi    '(display (midi-list-ports))'
run guile  guile_midi '(display (midi-list-ports))'
run pktpy  pktpy_midi 'import midi; print(midi.list_ports())'

# forth opens by name; the open message prints the matched label
run forth-open-as forth_midi "midi-open-as aseqdump"

if [ -x "$BUILD_DIR/mhs-midi" ]; then
    cat > "$TMP/PortLabels.hs" <<'EOF'
module PortLabels(main) where
import Midi
main :: IO ()
main = do
    n <- midiListPorts
    mapM_ (\i -> midiPortName i >>= putStrLn) [0 .. n - 1]
EOF
    check mhs "$(env MHSDIR="$PROJECT_ROOT/thirdparty/MicroHs" timeout 120 \
        "$BUILD_DIR/mhs-midi" -r -C -i"$PROJECT_ROOT/projects/mhs-midi/lib" -i"$TMP" PortLabels 2>&1)"
else
    echo "  SKIP: mhs (not built)"
fi

exit $FAIL
