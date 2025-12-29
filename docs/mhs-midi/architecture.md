# mhs-midi Architecture

This document explains how mhs-midi integrates MicroHs with MIDI functionality.

## Overview

```text
+------------------+     +------------------+     +------------------+
|  Music.hs        |     |   Midi.hs        | --> |   midi_ffi.c     |
| (Pure DSL)       |     |  (FFI bindings)  |     | (C implementation)|
+------------------+     +------------------+     +------------------+
        |                        |                        |
        v                        v                        v
+------------------+     +------------------+     +------------------+
| MusicPerform.hs  |     | MidiPerform.hs   |     |   libremidi      |
| (perform bridge) |     | (IO + generative)|     | (MIDI backend)   |
+------------------+     +------------------+     +------------------+
```

## Module Structure

| Module | Purpose | Dependencies |
|--------|---------|--------------|
| `Music.hs` | Pure music theory + DSL | None |
| `Midi.hs` | FFI bindings | None |
| `MusicPerform.hs` | Bridge pure Music to MIDI | Music, Midi |
| `MidiPerform.hs` | Immediate IO + generative | Midi only |

### Music.hs - Pure Music Theory

Pure functions and data types with no IO:

- Types: `Pitch`, `Duration`, `Velocity`, `Event`, `Music`
- Pitch constants: `c0`-`c8`, `cs0`-`cs8`, etc.
- Duration constants: `whole`, `half`, `quarter`, etc.
- Velocity constants: `ppp` through `fff`
- 55 scale constants: `scaleMajor`, `scaleDorian`, etc.
- Music constructors: `note`, `rest`, `chord`, `line`
- Combinators: `(+:+)`, `(|||)`, `timesM`
- Transformations: `transpose`, `louder`, `softer`, `stretch`, `compress`

### Midi.hs - FFI Bindings

Low-level C FFI bindings:

```haskell
foreign import ccall "midi_ffi.h midi_note_on"
    c_midi_note_on :: CInt -> CInt -> CInt -> IO CInt
```

Exports: `midiInit`, `midiOpen`, `midiClose`, `midiNoteOn`, `midiNoteOff`, `midiCC`, `midiProgram`, `midiPitchBend`, `midiSleep`, `midiPanic`, `midiRandomRange`, etc.

### MusicPerform.hs - Performance Bridge

Imports Music.hs and Midi.hs, provides:

- `perform :: Music -> IO ()` - perform on channel 1
- `performOn :: Channel -> Music -> IO ()` - perform on specific channel
- Microtonal helpers: `centsToBend`, `pitchBendCents`

### MidiPerform.hs - Immediate IO

Imports only Midi.hs (no Music.hs), so musical terms are IO actions:

- Direct playback: `note`, `chord`, `rest`, `melody`, `arpeggio`
- Generative: `pick`, `drunk`, `walk`, `euclidean`, `scramble`
- Scales: `major`, `minor`, `pentatonic`, `blues`, etc.
- Control: `open`, `close`, `panic`, `ports`

## C Layer

### midi_ffi.c - C Implementation

Location: `projects/mhs-midi/midi_ffi.c`

C wrapper around libremidi providing a simple interface:

```c
int midi_note_on(int channel, int pitch, int velocity);
int midi_open_virtual(const char* name);
void midi_close(void);
int midi_random_range(int min, int max);
```

### libremidi - MIDI Backend

Location: `thirdparty/libremidi/`

Cross-platform MIDI library supporting:

- macOS: CoreMIDI
- Linux: ALSA
- Windows: WinMM

### midi_ffi_wrappers.c - MicroHs FFI Glue

Location: `projects/mhs-midi/midi_ffi_wrappers.c`

Bridges between MicroHs runtime and midi_ffi.c:

```c
// MicroHs FFI wrapper pattern
from_t mhs_midi_note_on(int s) {
    return mhs_from_Int(s, 3, midi_note_on(
        mhs_to_Int(s, 0),   // channel
        mhs_to_Int(s, 1),   // pitch
        mhs_to_Int(s, 2))); // velocity
}
```

### xffi_table - FFI Registration

MicroHs uses two FFI tables:

1. **ffi_table** (built-in): Standard functions in `eval.c`
2. **xffi_table** (external): Custom FFI functions

mhs-midi populates `xffi_table` with MIDI functions:

```c
const struct ffi_entry midi_ffi_table[] = {
    { "midi_init",         0, mhs_midi_init },
    { "midi_note_on",      3, mhs_midi_note_on },
    { "midi_random_range", 2, mhs_midi_random_range },
    // ...
    { 0, 0, 0 }  // sentinel
};

const struct ffi_entry *xffi_table = midi_ffi_table;
```

## Build Process

### REPL Build (`mhs-midi`)

```text
generated/mhs.c (MicroHs compiler)
         +
     eval.c (runtime)
         +
midi_ffi_wrappers.c (FFI glue)
         +
    midi_ffi.c (C bindings)
         +
   libremidi.a (MIDI backend)
         |
         v
    mhs-midi (executable)
```

### Compiled Program Build

```text
MyProgram.hs + Music.hs + MusicPerform.hs
         |
         v (mhs -C)
    MyProgram.c (generated)
         +
     eval.c
         +
    midi_ffi.c
         +
   libremidi.a
         |
         v
   my_program (executable)
```

## File Locations

```text
projects/mhs-midi/
  lib/
    Music.hs             # Pure music theory + DSL
    Midi.hs              # FFI bindings
    MusicPerform.hs      # perform bridge
    MidiPerform.hs       # Immediate IO + generative
  examples/
    HelloMidi.hs         # Example programs
    Chords.hs
    Melody.hs
    Arpeggio.hs
  midi_ffi.h             # C API header
  midi_ffi.c             # C implementation
  midi_ffi_wrappers.c    # MicroHs FFI wrappers
  mhs_midi_main.c        # REPL entry point
  CMakeLists.txt         # Build configuration

scripts/
  mhs-midi-repl          # REPL launcher
  mhs-midi-compile       # Compilation script

thirdparty/
  MicroHs/               # MicroHs compiler
  libremidi/             # MIDI backend
```

## FFI Function Signature Reference

### Argument Extraction

```c
mhs_to_Int(s, n)   // Extract Int from position n
mhs_to_Ptr(s, n)   // Extract pointer (for strings)
```

### Return Value

```c
mhs_from_Int(s, arity, value)   // Return Int
mhs_from_Ptr(s, arity, ptr)     // Return pointer
mhs_from_Unit(s, arity)         // Return () for void functions
```

### Arity

The arity parameter must equal the number of arguments consumed:

| Function | Args | Arity |
|----------|------|-------|
| `midi_init` | 0 | 0 |
| `midi_open` | 1 | 1 |
| `midi_note_off` | 2 | 2 |
| `midi_note_on` | 3 | 3 |
| `midi_random_range` | 2 | 2 |

## Caching

MicroHs supports compilation caching via `.mhscache`:

- `-C` flag enables read/write caching
- First run builds the cache
- Subsequent runs are faster

The `mhs-midi-repl` script enables caching by default.
