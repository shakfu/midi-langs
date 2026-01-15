# Joy-MIDI

Joy-MIDI is a MIDI-enabled Joy interpreter for musical composition. Joy is a purely functional, stack-based concatenative programming language where composition is achieved through function concatenation rather than application.

## Quick Start

```bash
# Build
cmake -B build && cmake --build build --target joy_midi

# Run interactively
./build/joy_midi

# Run a script
./build/joy_midi script.joy
```

## Basic Usage

### Interactive REPL

```
$ ./build/joy_midi
Joy-MIDI  -  Joy interpreter with MIDI extensions
Type 'quit' to exit, 'help' for MIDI words
Created virtual MIDI output: JoyMIDI
> c d e
60 62 64
> [c e g]
[60 64 67]
> [c d e] [7 +] map
[67 69 71]
```

### Musical Notation

Notes are converted to MIDI integers at **parse time**. This means `[c d e]` directly becomes `[60 62 64]`, enabling seamless use of Joy combinators.

```joy
\ Notes are just integers
c d e           \ Stack: 60 62 64
[c d e]         \ List: [60 62 64]

\ Octaves (default is 4)
c c5 c6         \ 60 72 84
c3 c4 c5        \ 48 60 72

\ Accidentals
c+ c- d+ d-     \ 61 59 63 61 (sharp/flat)
c+5             \ 73 (C#5)

\ Play notes
c play          \ Play middle C
[c e g] play    \ Play arpeggio
[c e g] chord   \ Play chord

\ Transform with Joy combinators
[c d e] [7 +] map play      \ Transpose up a fifth
[c d e] [12 +] map play     \ Transpose up an octave
[c d e f g] [64 >] filter   \ Filter high notes
```

### Notation Reference

| Notation | Example | Description |
|----------|---------|-------------|
| Note | `c`, `d`, `e` | Note in octave 4 |
| Octave | `c5`, `d6` | Note in specific octave |
| Sharp | `c+`, `f+` | Raise by semitone |
| Flat | `c-`, `b-` | Lower by semitone |
| Rest | `r` | Rest marker (-1) |
| Dynamics | `pp`, `mf`, `ff` | Set velocity (runtime) |

### Full Example

```joy
120 tempo
mf

\ Play a scale - notes become integers in the list
[c d e f g a b c5] play

\ Chord progression
[c e g] chord       \ C major
[f a c5] chord      \ F major
[g b d5] chord      \ G major
[c e g] chord       \ C major

\ Transpose a melody
[c d e f g] [7 +] map play
```

### Verbose MIDI Primitives

For more control, use explicit MIDI primitives:

```joy
\ Play middle C (pitch 60, velocity 80, duration 500ms)
60 80 500 midi-note

\ Play a C major chord
60 major 80 500 midi-chord

\ Using pitch names
"C4" pitch 80 500 midi-note
```

## Language Basics

Joy is a concatenative language where programs are built by composing functions. Data flows through a stack, and functions consume and produce stack values.

### Stack Operations

```joy
1 2 3       \ Push 1, 2, 3 onto stack
dup         \ Duplicate top
swap        \ Swap top two
pop         \ Remove top
```

### Arithmetic

```joy
3 4 + .     \ 7
10 3 - .    \ 7
6 7 * .     \ 42
```

### Lists and Combinators

```joy
[1 2 3] first .            \ 1
[1 2 3] rest .             \ [2 3]
[1 2 3] [2 *] map .        \ [2 4 6]
[1 2 3 4 5] [3 >] filter . \ [4 5]
[1 2 3 4] 0 [+] fold .     \ 10
```

## MIDI Primitives

### Port Management

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `midi-list` | `( -- )` | List available MIDI output ports |
| `midi-virtual` | `( -- )` | Create virtual port "JoyMIDI" |
| `midi-open` | `( n -- )` | Open port by index |
| `midi-close` | `( -- )` | Close current port |

### Musical Notation Playback

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `play` | `( pitch -- )` or `( [pitches] -- )` | Play note(s) sequentially |
| `chord` | `( pitch -- )` or `( [pitches] -- )` | Play note(s) simultaneously |

### Note Operations

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `midi-note` | `( pitch vel dur -- )` | Play note (blocking) |
| `midi-note-on` | `( pitch vel -- )` | Send note-on message |
| `midi-note-off` | `( pitch -- )` | Send note-off message |
| `midi-chord` | `( [pitches] vel dur -- )` | Play chord (blocking) |

### Utilities

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `midi-sleep` | `( ms -- )` | Sleep for milliseconds |
| `pitch` | `( "name" -- n )` | Parse pitch name to MIDI number |
| `tempo` | `( bpm -- )` | Set tempo in BPM |
| `quant` | `( pct -- )` | Set note gate time (0-100%) |
| `vol` | `( pct -- )` | Set velocity (0-100%) |

### Music Theory

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `major` | `( root -- [pitches] )` | Build major triad |
| `minor` | `( root -- [pitches] )` | Build minor triad |
| `dim` | `( root -- [pitches] )` | Build diminished triad |
| `aug` | `( root -- [pitches] )` | Build augmented triad |
| `dom7` | `( root -- [pitches] )` | Build dominant 7th |
| `maj7` | `( root -- [pitches] )` | Build major 7th |
| `min7` | `( root -- [pitches] )` | Build minor 7th |
| `transpose` | `( pitch n -- pitch' )` | Transpose by semitones |

## Examples

### Melody with Transposition

```joy
\ Define a melody
[c d e f g]

\ Play it, then transpose and play again
dup play
[12 +] map play   \ Up an octave
```

### Chord Progression

```joy
[c e g] chord       \ I   (C major)
[f a c5] chord      \ IV  (F major)
[g b d5] chord      \ V   (G major)
[c e g] chord       \ I   (C major)
```

### Generative Music

```joy
\ Random note from C major scale
[c d e f g a b] dup size rand swap rem at play

\ Play 10 random notes
10 [
  [c d e f g a b] dup size rand swap rem at play
] times
```

## Looping

```joy
\ times - repeat N times
5 [c play] times

\ step - iterate over a list
[c d e f g] [play] step

\ map - transform each element
[c d e] [12 +] map    \ -> [72 74 76]

\ fold - reduce a list
[1 2 3 4] 0 [+] fold  \ -> 10

\ while - loop with condition
0 [dup 5 <] [dup . succ] while pop
```

For music, step and times are most useful:

```
\ Play a scale
[c d e f g a b c5] [play] step

\ Play a chord 4 times
4 [[c e g] chord] times

\ Play 8 random notes
8 [[c d e f g a b] dup size rand swap rem at play] times
```

## User Definitions

Define your own words using `DEFINE` or `def`:

```joy
\ Single definition
def double == 2 * .
5 double              \ -> 10

\ Multiple definitions
DEFINE
  pick == dup size rand swap rem at ;
  cmaj == [c e g] .

\ Use them
[c d e f g a b] pick play    \ Random note
cmaj chord                    \ C major chord

\ Musical patterns
def arp == [play] step .
def loop4 == 4 swap times .

[c e g] arp                   \ Arpeggio
[[c e g] chord] loop4         \ Chord 4 times
```

Syntax:
- `def name == body .` - define a single word
- `DEFINE name == body .` - same as def
- `DEFINE n1 == b1 ; n2 == b2 .` - multiple definitions

## Architecture

Joy-MIDI is built on the pyjoy-runtime, a clean C implementation of the Joy language:

```
projects/joy-midi/
  main.c              - Entry point, REPL setup
  joy_midi.c/h        - Primitive registration
  midi_primitives.c/h - MIDI implementations
  music_notation.c/h  - Note parsing, play/chord
  music_context.c/h   - Musical state (tempo, velocity)

thirdparty/pyjoy-runtime/
  joy_runtime.c/h     - Core Joy runtime
  joy_primitives.c    - Standard Joy primitives
  joy_parser.c/h      - Tokenizer and parser (with note transformer)
```

### Design Philosophy

Notes are converted to MIDI integers at **parse time**, not execution time. This enables Joy's compositional model without any special combinators:

```joy
[c d e]             \ Directly becomes [60 62 64]
[c d e] [7 +] map   \ Transpose: [67 69 71]
[c e g] chord       \ Play C major chord
```

Octave is explicit (default 4), not stateful:
- `c` = C4 = 60
- `c5` = C5 = 72
- `c 12 +` = C5 = 72 (arithmetic works too)

## See Also

- [Joy Language](https://hypercubed.github.io/joy/html/j00rat.html) - Original Joy documentation
- [Alda](https://alda.io/) - Music composition language that inspires Joy-MIDI's notation
