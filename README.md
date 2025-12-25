# Forth

A Forth-like interpreter with MIDI capabilities for generating and transforming MIDI sequences.

## Overview

This project contains two interpreters:

- **forth** - A minimal Forth-like interpreter with basic arithmetic, stack manipulation, and bitwise operations
- **midi_forth** - Extended interpreter with real-time MIDI output via libremidi

## Building

### Prerequisites

- GCC or Clang
- CMake 3.22+ (for libremidi)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)

### Build Commands

```bash
make              # Build both interpreters
make forth        # Build basic interpreter only
make midi_forth   # Build MIDI interpreter only
make clean        # Remove binaries and build artifacts
make test         # Build and run basic test
```

The first build of `midi_forth` will automatically compile libremidi from `thirdparty/libremidi`.

## Running

```bash
./forth           # Run basic interpreter (shows examples, then REPL)
./midi_forth      # Run MIDI interpreter (REPL only)
```

Type `quit` to exit either interpreter.

## MIDI Forth

### Quick Start

```forth
midi-virtual                              \ create virtual MIDI port
1 60 100 note-on 500 ms 1 60 0 note-off   \ play middle C for 500ms
midi-close                                \ close port
```

Connect a DAW or software synth to the "MidiForth" virtual port to hear output.

### MIDI Words

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `midi-list` | ( -- ) | List available MIDI output ports |
| `midi-open` | ( n -- ) | Open output port by index |
| `midi-virtual` | ( -- ) | Create virtual output port "MidiForth" |
| `midi-close` | ( -- ) | Close the current output port |
| `note-on` | ( channel pitch velocity -- ) | Send Note On message |
| `note-off` | ( channel pitch velocity -- ) | Send Note Off message |
| `cc` | ( channel cc-number value -- ) | Send Control Change message |
| `panic` | ( -- ) | Send All Notes Off on all channels |

**Notes:**
- Channel range: 1-16 (converted internally to 0-15)
- Pitch range: 0-127 (60 = middle C)
- Velocity range: 0-127

### Utility Words

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `ms` | ( n -- ) | Sleep for n milliseconds |
| `.s` | ( -- ) | Display stack contents without modifying |
| `help` | | Show available MIDI commands |

### Examples

**Play a C major chord:**
```forth
midi-virtual
1 60 100 note-on    \ C
1 64 100 note-on    \ E
1 67 100 note-on    \ G
1000 ms
1 60 0 note-off
1 64 0 note-off
1 67 0 note-off
midi-close
```

**Play a scale:**
```forth
midi-virtual
1 60 100 note-on 200 ms 1 60 0 note-off
1 62 100 note-on 200 ms 1 62 0 note-off
1 64 100 note-on 200 ms 1 64 0 note-off
1 65 100 note-on 200 ms 1 65 0 note-off
1 67 100 note-on 200 ms 1 67 0 note-off
1 69 100 note-on 200 ms 1 69 0 note-off
1 71 100 note-on 200 ms 1 71 0 note-off
1 72 100 note-on 200 ms 1 72 0 note-off
midi-close
```

**Send CC (e.g., mod wheel):**
```forth
midi-virtual
1 1 64 cc    \ channel 1, CC 1 (mod wheel), value 64
midi-close
```

## Core Forth Words

Both interpreters support these standard Forth words:

### Arithmetic
| Word | Stack Effect | Description |
|------|--------------|-------------|
| `+` | ( a b -- sum ) | Addition |
| `-` | ( a b -- diff ) | Subtraction |
| `*` | ( a b -- product ) | Multiplication |
| `/` | ( a b -- quotient ) | Integer division |

### Stack Manipulation
| Word | Stack Effect | Description |
|------|--------------|-------------|
| `dup` | ( a -- a a ) | Duplicate top |
| `drop` | ( a -- ) | Discard top |
| `swap` | ( a b -- b a ) | Swap top two |
| `over` | ( a b -- a b a ) | Copy second to top |
| `rot` | ( a b c -- b c a ) | Rotate top three |

### Comparison
| Word | Stack Effect | Description |
|------|--------------|-------------|
| `=` | ( a b -- flag ) | Equal (-1 true, 0 false) |
| `<` | ( a b -- flag ) | Less than |
| `>` | ( a b -- flag ) | Greater than |

### Bitwise
| Word | Stack Effect | Description |
|------|--------------|-------------|
| `and` | ( a b -- a&b ) | Bitwise AND |
| `or` | ( a b -- a\|b ) | Bitwise OR |
| `xor` | ( a b -- a^b ) | Bitwise XOR |
| `not` | ( a -- ~a ) | Bitwise NOT |

### Output
| Word | Stack Effect | Description |
|------|--------------|-------------|
| `.` | ( n -- ) | Print and pop top of stack |
| `cr` | ( -- ) | Print newline |
| `space` | ( -- ) | Print space |

## Architecture

### forth.c

Minimal Forth interpreter (~370 lines):
- Fixed-size int32_t stack (100 elements)
- Dictionary of word structs mapping names to C function pointers
- Tokenizer splits input by whitespace
- Numbers are pushed to stack; words are looked up and executed

### midi_forth.c

Extended interpreter (~500 lines) adding:
- libremidi integration via C API
- MIDI observer for port enumeration
- MIDI output handle for sending messages
- Virtual port support for software routing
- Automatic cleanup on exit (sends panic, frees resources)

### Dependencies

- **libremidi** v5.3.1 - Cross-platform MIDI I/O library
  - Location: `thirdparty/libremidi/`
  - Built as static library during first `make midi_forth`
  - Uses CoreMIDI on macOS, ALSA on Linux, WinMM on Windows

## License

See individual component licenses:
- libremidi: See `thirdparty/libremidi/LICENSE.md`
