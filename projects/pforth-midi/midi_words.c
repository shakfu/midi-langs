/* midi_words.c - C words for pforth-midi, registered through pForth's
 * CustomFunctionTable. Words named (X) return an ior (0 = ok) that the
 * Forth wrappers in midi.fth turn into an abort; see that file for the
 * user-facing stack effects. */

#include "pf_all.h"
#include "midi_ffi.h"
#include "music_theory.h"

#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#include <io.h>
#define isatty _isatty
#define STDIN_FILENO 0
#else
#include <unistd.h>
#endif

#define MAX_INTERVALS 32

/* Copy a Forth string to a NUL-terminated buffer; returns buf, or NULL if it does not fit. */
static char* cstr(cell_t addr, cell_t len, char* buf, size_t size) {
    if (len < 0 || (size_t)len >= size) return NULL;
    memcpy(buf, (const char*)addr, (size_t)len);
    buf[len] = '\0';
    return buf;
}

/* Copy a Forth interval table ( addr n ) to ints; returns n, or -1 if too long. */
static int intervals(cell_t addr, cell_t n, int* out) {
    const cell_t* cells = (const cell_t*)addr;
    if (n < 0 || n > MAX_INTERVALS) return -1;
    for (cell_t i = 0; i < n; i++) out[i] = (int)cells[i];
    return (int)n;
}

static cell_t flag(int b) { return b ? -1 : 0; }

/* Ports */

static void midi_list(void) {
    int n = midi_list_ports();
    if (n == 0) {
        printf("  (none - use midi-open to create a virtual port)\n");
    }
    for (int i = 0; i < n; i++) {
        printf("  %d: %s\n", i, midi_port_name(i));
    }
}

static cell_t midi_port_count(void) { return midi_list_ports(); }

static cell_t open_port(cell_t index) {
    midi_list_ports();
    return midi_open((int)index) == 0 ? 0 : -1;
}

static cell_t open_virtual(cell_t addr, cell_t len) {
    char name[256];
    if (!cstr(addr, len, name, sizeof(name))) return -1;
    return midi_open_virtual(name) == 0 ? 0 : -1;
}

static void close_port(void) { midi_close(); }
static cell_t is_open(void) { return flag(midi_is_open()); }

/* Messages; channels are 1-16 */

static cell_t note_on(cell_t pitch, cell_t vel, cell_t ch) {
    return midi_note_on((int)ch, (int)pitch, (int)vel) == 0 ? 0 : -1;
}

static cell_t note_off(cell_t pitch, cell_t ch) {
    return midi_note_off((int)ch, (int)pitch) == 0 ? 0 : -1;
}

static cell_t cc(cell_t ch, cell_t ctl, cell_t val) {
    return midi_cc((int)ch, (int)ctl, (int)val) == 0 ? 0 : -1;
}

static cell_t program(cell_t ch, cell_t prog) {
    return midi_program((int)ch, (int)prog) == 0 ? 0 : -1;
}

/* val is 0-16383 with 8192 = centre, as in the spec; midi_ffi takes -8192..8191 */
static cell_t pitch_bend(cell_t ch, cell_t val) {
    if (val < 0 || val > 16383) return -1;
    return midi_pitch_bend((int)ch, (int)val - 8192) == 0 ? 0 : -1;
}

static cell_t all_notes_off(cell_t ch) {
    return midi_cc((int)ch, 123, 0) == 0 ? 0 : -1;
}

static void panic(void) { midi_panic(); }

static cell_t cents_to_bend(cell_t cents) { return midi_cents_to_bend((int)cents); }

/* Pitches */

static cell_t parse_pitch(cell_t addr, cell_t len) {
    char buf[16];
    if (!cstr(addr, len, buf, sizeof(buf))) return -1;
    return music_parse_pitch(buf);
}

static void dot_pitch(cell_t pitch) {
    char buf[16];
    if (music_pitch_to_name((int)pitch, buf, sizeof(buf), 1)) {
        printf("%s ", buf);
    } else {
        printf("?%ld ", (long)pitch);
    }
}

/* Scales: ( root addr n ... ) where addr n is a Forth interval table */

static cell_t scale_degree(cell_t root, cell_t addr, cell_t n, cell_t degree) {
    int iv[MAX_INTERVALS];
    int count = intervals(addr, n, iv);
    if (count <= 0) return -1;
    return music_scale_degree((int)root, iv, count, (int)degree);
}

static cell_t in_scale(cell_t pitch, cell_t root, cell_t addr, cell_t n) {
    int iv[MAX_INTERVALS];
    int count = intervals(addr, n, iv);
    if (count <= 0) return 0;
    return flag(music_in_scale((int)pitch, (int)root, iv, count));
}

static cell_t quantize(cell_t pitch, cell_t root, cell_t addr, cell_t n) {
    int iv[MAX_INTERVALS];
    int count = intervals(addr, n, iv);
    if (count <= 0) return pitch;
    return music_quantize_to_scale((int)pitch, (int)root, iv, count);
}

/* Recording and files */

static void record_start(cell_t bpm) { midi_record_start((int)bpm); }
static cell_t record_stop(void) { return midi_record_stop(); }
static cell_t record_count(void) { return midi_record_count(); }
static cell_t is_recording(void) { return flag(midi_record_active()); }

static cell_t write_mid(cell_t addr, cell_t len) {
    char path[1024];
    if (!cstr(addr, len, path, sizeof(path))) return -1;
    return midi_save_mid(path) == 0 ? 0 : -1;
}

static cell_t read_mid(cell_t addr, cell_t len) {
    char path[1024];
    if (!cstr(addr, len, path, sizeof(path))) return -1;
    return midi_read_mid(path) == 0 ? 0 : -1;
}

/* Random */

static void seed(cell_t n) { midi_seed_random((int)n); }
static cell_t random_range(cell_t lo, cell_t hi) { return midi_random_range((int)lo, (int)hi); }

/* Terminal */

static cell_t stdin_tty(void) { return flag(isatty(STDIN_FILENO)); }

/* Order must match CompileCustomFunctions below. */
CFunc0 CustomFunctionTable[] = {
    (CFunc0)midi_list,
    (CFunc0)midi_port_count,
    (CFunc0)open_port,
    (CFunc0)open_virtual,
    (CFunc0)close_port,
    (CFunc0)is_open,
    (CFunc0)note_on,
    (CFunc0)note_off,
    (CFunc0)cc,
    (CFunc0)program,
    (CFunc0)pitch_bend,
    (CFunc0)all_notes_off,
    (CFunc0)panic,
    (CFunc0)cents_to_bend,
    (CFunc0)parse_pitch,
    (CFunc0)dot_pitch,
    (CFunc0)scale_degree,
    (CFunc0)in_scale,
    (CFunc0)quantize,
    (CFunc0)record_start,
    (CFunc0)record_stop,
    (CFunc0)record_count,
    (CFunc0)is_recording,
    (CFunc0)write_mid,
    (CFunc0)read_mid,
    (CFunc0)seed,
    (CFunc0)random_range,
    (CFunc0)stdin_tty,
};

Err CompileCustomFunctions(void) {
    static const struct {
        const char* name;
        int mode;
        int params;
    } words[] = {
        {"MIDI-LIST", C_RETURNS_VOID, 0},
        {"MIDI-PORT-COUNT", C_RETURNS_VALUE, 0},
        {"(MIDI-OPEN-PORT)", C_RETURNS_VALUE, 1},
        {"(MIDI-OPEN-VIRTUAL)", C_RETURNS_VALUE, 2},
        {"MIDI-CLOSE", C_RETURNS_VOID, 0},
        {"MIDI-OPEN?", C_RETURNS_VALUE, 0},
        {"(NOTE-ON)", C_RETURNS_VALUE, 3},
        {"(NOTE-OFF)", C_RETURNS_VALUE, 2},
        {"(CC)", C_RETURNS_VALUE, 3},
        {"(PROGRAM)", C_RETURNS_VALUE, 2},
        {"(PITCH-BEND)", C_RETURNS_VALUE, 2},
        {"(ALL-NOTES-OFF)", C_RETURNS_VALUE, 1},
        {"PANIC", C_RETURNS_VOID, 0},
        {"CENTS>BEND", C_RETURNS_VALUE, 1},
        {"(PARSE-PITCH)", C_RETURNS_VALUE, 2},
        {".PITCH", C_RETURNS_VOID, 1},
        {"SCALE-DEGREE", C_RETURNS_VALUE, 4},
        {"IN-SCALE?", C_RETURNS_VALUE, 4},
        {"QUANTIZE", C_RETURNS_VALUE, 4},
        {"RECORD-START", C_RETURNS_VOID, 1},
        {"RECORD-STOP", C_RETURNS_VALUE, 0},
        {"RECORD-COUNT", C_RETURNS_VALUE, 0},
        {"RECORDING?", C_RETURNS_VALUE, 0},
        {"(WRITE-MID)", C_RETURNS_VALUE, 2},
        {"(READ-MID)", C_RETURNS_VALUE, 2},
        {"SEED", C_RETURNS_VOID, 1},
        {"RANDOM-RANGE", C_RETURNS_VALUE, 2},
        {"STDIN-TTY?", C_RETURNS_VALUE, 0},
    };
    for (size_t i = 0; i < sizeof(words) / sizeof(words[0]); i++) {
        Err err = CreateGlueToC(words[i].name, (ucell_t)i, words[i].mode, words[i].params);
        if (err < 0) return err;
    }
    return 0;
}
