# Joy-MIDI Alda-like Notation Implementation Plan

## Overview

Add Alda-like musical notation to Joy-MIDI, enabling concise syntax like `c d e f g` instead of `"C4" pitch 80 500 midi-note`. The implementation intercepts undefined symbols at execution time and parses them as musical notation.

## Architecture

**Interception Point**: Modify `joy_execute_symbol()` in joy_runtime.c to call a music notation handler when a symbol is not found in the dictionary.

**Design Principle**: Keep musical notation parsing in the joy-midi project, not in pyjoy-runtime. The runtime provides a hook; joy-midi implements the music-specific logic.

## Files to Modify

### 1. `thirdparty/pyjoy-runtime/joy_runtime.h`

Add a callback hook for undefined symbol handling:

```c
/* Callback for handling undefined symbols (returns true if handled) */
typedef bool (*JoyUndefHandler)(JoyContext* ctx, const char* name);

struct JoyContext {
    JoyStack* stack;
    JoyDict* dictionary;
    bool trace_enabled;
    int autoput;
    int undeferror;
    int echo;
    JoyUndefHandler undef_handler;  /* NEW: custom handler for undefined symbols */
    void* user_data;                /* NEW: user data (e.g., MusicContext*) */
};
```

### 2. `thirdparty/pyjoy-runtime/joy_runtime.c`

Modify `joy_execute_symbol()` to call the handler:

```c
void joy_execute_symbol(JoyContext* ctx, const char* name) {
    JoyWord* word = joy_dict_lookup(ctx->dictionary, name);
    if (!word) {
        /* Try custom handler first */
        if (ctx->undef_handler && ctx->undef_handler(ctx, name)) {
            return;  /* Handler processed it */
        }
        fprintf(stderr, "Undefined word: %s\n", name);
        joy_error("Undefined word");
    }
    // ... rest unchanged
}
```

Initialize new fields in `joy_context_new()`:
```c
ctx->undef_handler = NULL;
ctx->user_data = NULL;
```

## Files to Create

### 3. `projects/joy-midi/music_context.h`

Musical state structure:

```c
typedef struct {
    int octave;           /* Current octave (0-9, default 4) */
    int duration_ms;      /* Current duration in ms (default 500) */
    int velocity;         /* Current velocity (0-127, default 80) */
    int tempo;            /* BPM (default 120) */
    int quantization;     /* Gate percentage (0-100, default 90) */
    int last_pitch;       /* Last played pitch for ties */
    bool in_chord;        /* Currently building a chord */
    int chord_pitches[16];/* Pitches for current chord */
    int chord_count;      /* Number of pitches in chord */
} MusicContext;

MusicContext* music_context_new(void);
void music_context_free(MusicContext* mctx);
```

### 4. `projects/joy-midi/music_notation.h`

```c
/* Parse and execute musical notation. Returns true if handled. */
bool music_handle_symbol(JoyContext* ctx, const char* name);

/* Initialize music notation system */
void music_notation_init(JoyContext* ctx);
```

### 5. `projects/joy-midi/music_notation.c`

Core notation parser (~400 lines):

```c
bool music_handle_symbol(JoyContext* ctx, const char* name) {
    MusicContext* mctx = (MusicContext*)ctx->user_data;

    /* Try parsing as: */

    /* 1. Octave commands: o0-o9, >>, << */
    if (parse_octave_command(mctx, name)) return true;

    /* 2. Rest: r, r4, r8, etc. */
    if (parse_rest(ctx, mctx, name)) return true;

    /* 3. Dynamic: ppp, pp, p, mp, mf, f, ff, fff */
    if (parse_dynamic(mctx, name)) return true;

    /* 4. Note with optional duration/accidental/dot: c, c4, c+, c4., c+4. */
    if (parse_note(ctx, mctx, name)) return true;

    /* 5. Chord notation: c/e/g, c:maj, c:min7 */
    if (parse_chord(ctx, mctx, name)) return true;

    return false;  /* Not music notation */
}
```

**Note Parsing Logic** (most complex):
```c
bool parse_note(JoyContext* ctx, MusicContext* mctx, const char* name) {
    const char* p = name;
    int note_offset = -1;
    int accidental = 0;
    int duration = 0;
    int dots = 0;

    /* Note letter: c d e f g a b */
    char c = tolower(*p);
    switch (c) {
        case 'c': note_offset = 0; break;
        case 'd': note_offset = 2; break;
        case 'e': note_offset = 4; break;
        case 'f': note_offset = 5; break;
        case 'g': note_offset = 7; break;
        case 'a': note_offset = 9; break;
        case 'b': note_offset = 11; break;
        default: return false;
    }
    p++;

    /* Accidentals: + (sharp), - (flat), _ (natural) */
    while (*p == '+' || *p == '-' || *p == '_') {
        if (*p == '+') accidental++;
        else if (*p == '-') accidental--;
        /* _ resets to natural (handled by key sig later) */
        p++;
    }

    /* Duration: 1, 2, 4, 8, 16, 32 */
    if (isdigit(*p)) {
        duration = atoi(p);
        while (isdigit(*p)) p++;
        /* Update default duration */
        mctx->duration_ms = duration_to_ms(duration, mctx->tempo);
    }

    /* Dots: . or .. */
    while (*p == '.') {
        dots++;
        p++;
    }

    /* Must have consumed entire string */
    if (*p != '\0') return false;

    /* Calculate MIDI pitch */
    int pitch = (mctx->octave + 1) * 12 + note_offset + accidental;
    if (pitch < 0) pitch = 0;
    if (pitch > 127) pitch = 127;

    /* Calculate actual duration with dots */
    int dur = mctx->duration_ms;
    for (int i = 0; i < dots; i++) {
        dur = dur * 3 / 2;  /* Each dot adds 50% */
    }

    /* Apply quantization */
    int play_dur = dur * mctx->quantization / 100;

    /* Play the note */
    play_note(pitch, mctx->velocity, play_dur);

    /* Wait remaining duration */
    int rest_dur = dur - play_dur;
    if (rest_dur > 0) usleep(rest_dur * 1000);

    mctx->last_pitch = pitch;
    return true;
}
```

**Duration Conversion:**
```c
int duration_to_ms(int duration, int tempo) {
    /* At 120 BPM, quarter note = 500ms */
    /* duration: 1=whole, 2=half, 4=quarter, 8=eighth, etc. */
    int quarter_ms = 60000 / tempo;
    return quarter_ms * 4 / duration;
}
```

## Files to Modify

### 6. `projects/joy-midi/main.c`

Initialize music notation:

```c
#include "music_notation.h"

int main(...) {
    JoyContext* ctx = joy_context_new();
    joy_runtime_init(ctx);

    /* Initialize music notation system */
    music_notation_init(ctx);  /* NEW */

    joy_midi_register_primitives(ctx);
    // ...
}
```

### 7. `projects/joy-midi/midi_primitives.c`

Add new primitives for attributes:

```c
/* (tempo N) - set tempo */
void prim_tempo(JoyContext* ctx) {
    REQUIRE(1, "tempo");
    JoyValue v = POP();
    EXPECT_TYPE(v, JOY_INTEGER, "tempo");
    MusicContext* mctx = ctx->user_data;
    mctx->tempo = (int)v.data.integer;
}

/* (quant N) - set quantization 0-100 */
void prim_quant(JoyContext* ctx) { ... }

/* (vol N) - set velocity 0-100 (scaled to 0-127) */
void prim_vol(JoyContext* ctx) { ... }
```

### 8. `projects/joy-midi/CMakeLists.txt`

Add new source files:
```cmake
add_executable(joy_midi
    main.c
    joy_midi.c
    midi_primitives.c
    music_context.c    # NEW
    music_notation.c   # NEW
)
```

## Implementation Phases

### Phase 1: Core Infrastructure (First)
1. Add `undef_handler` and `user_data` to JoyContext
2. Create MusicContext struct
3. Create basic music_notation.c skeleton
4. Test: `c` should be recognized (even if not playing yet)

### Phase 2: Basic Notes
1. Implement note letter parsing (c d e f g a b)
2. Implement octave state and commands (o4, >, <)
3. Play notes using existing MIDI infrastructure
4. Test: `o4 c d e f g` should play C major scale

### Phase 3: Duration and Accidentals
1. Add duration suffix parsing (c4, c8, etc.)
2. Add accidental parsing (c+, c-, c_)
3. Add dotted note support (c4., c4..)
4. Test: `c4 d8 e8 f4. g2` should play varied rhythm

### Phase 4: Rests and Dynamics
1. Implement rest parsing (r, r4, r8)
2. Add dynamic primitives (ppp through fff)
3. Add tempo primitive
4. Test: `mf c4 r4 ff e4 r2`

### Phase 5: Chords
1. Implement slash notation (c/e/g)
2. Implement named chords (c:maj, c:min)
3. Test: `c4/e/g d:min e:min f:maj`

## Notation Summary

| Notation | Example | Description |
|----------|---------|-------------|
| Note | `c`, `d`, `e` | Play note in current octave |
| Duration | `c4`, `c8` | Set/use duration (4=quarter) |
| Dotted | `c4.`, `c4..` | Add 50%/75% duration |
| Sharp | `c+`, `f+` | Raise by semitone |
| Flat | `b-`, `e-` | Lower by semitone |
| Octave set | `o4`, `o5` | Set octave |
| Octave up | `>>` | Raise octave by 1 |
| Octave down | `<<` | Lower octave by 1 |
| Rest | `r`, `r4` | Rest for duration |
| Chord slash | `c/e/g` | Simultaneous notes |
| Chord named | `c:maj` | Named chord |
| Dynamics | `pp`, `mf`, `ff` | Set velocity |
| Tempo | `tempo` primitive | `120 tempo` |
| Quantization | `quant` primitive | `90 quant` |

## Testing Strategy

### Unit Tests (tests/CMakeLists.txt)

```cmake
add_test(NAME joy_midi_note_c
    COMMAND sh -c "echo 'midi-virtual c' | $<TARGET_FILE:joy_midi> 2>&1 | grep -v 'Undefined'")

add_test(NAME joy_midi_scale
    COMMAND sh -c "echo 'midi-virtual o4 c d e f g a b >> c' | $<TARGET_FILE:joy_midi>")

add_test(NAME joy_midi_rhythm
    COMMAND sh -c "echo 'midi-virtual c4 d8 d e4. f2' | $<TARGET_FILE:joy_midi>")

add_test(NAME joy_midi_accidentals
    COMMAND sh -c "echo 'midi-virtual c c+ d d- e' | $<TARGET_FILE:joy_midi>")

add_test(NAME joy_midi_chord
    COMMAND sh -c "echo 'midi-virtual c/e/g' | $<TARGET_FILE:joy_midi>")
```

### Integration Test

Create `tests/examples/demo_joy_notation.joy`:
```joy
midi-virtual
120 tempo
mf

\ Scale
o4 c d e f g a b >> c

\ Rhythm
< c4 d8 d e4. f2

\ Chords
c/e/g d/f/a e/g/b

\ With dynamics
pp c d e
ff f g a b >> c
```

## Verification

1. Build: `cmake -B build && cmake --build build --target joy_midi`
2. Test basic: `echo 'midi-virtual c d e' | ./build/joy_midi`
3. Test scale: `echo 'midi-virtual o4 c d e f g a b >> c' | ./build/joy_midi`
4. Run tests: `ctest --test-dir build -R joy_midi`
