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
> 3 4 + .
7
> "C4" pitch .
60
> 60 major .
[60 64 67]
```

### Musical Notation (Alda-like)

Joy-MIDI supports concise Alda-like notation where **notes push MIDI pitches onto the stack**. Use `play` for sequential playback or `chord` for simultaneous notes.

A virtual MIDI port is created automatically when starting the REPL.

```joy
\ Notes push pitches onto the stack
c d e           \ Stack: 60 62 64

\ Use play for sequential playback
c play          \ Plays middle C
[c d e] i stack play    \ Play C D E sequentially

\ Use chord for simultaneous playback
[c e g] i stack chord   \ Play C major chord
c:maj chord             \ Same thing using named chord syntax

\ Named chords push pitch lists
c:maj           \ Stack: [60 64 67]
d:min           \ Stack: [62 65 69]
g:7             \ Stack: [67 71 74 77]
```

### Notation Reference

| Notation | Example | Description |
|----------|---------|-------------|
| Note | `c`, `d`, `e` | Push pitch in current octave |
| Duration | `c4`, `c8` | Set duration (4=quarter, 8=eighth) |
| Dotted | `c4.`, `c4..` | Add 50%/75% to duration |
| Sharp | `c+`, `f+` | Raise by semitone |
| Flat | `b-`, `e-` | Lower by semitone |
| Octave set | `o4`, `o5` | Set current octave |
| Octave up | `>>` | Raise octave by 1 |
| Octave down | `<<` | Lower octave by 1 |
| Rest | `r`, `r4` | Push rest marker |
| Named chord | `c:maj`, `d:min7` | Push chord as pitch list |
| Dynamics | `pp`, `mf`, `ff` | Set velocity (state change) |

### Full Example

```joy
120 tempo
mf

\ Play a scale
o4 c d e f g a b >> c
[c d e f g a b >> c] i stack play

\ Play chord progression (I-IV-V-I)
c:maj chord
f:maj chord
g:maj chord
c:maj chord

\ Transpose a melody up a fifth
[c d e] i [7 +] map stack play
```

### Verbose MIDI Primitives

For more control, use explicit MIDI primitives:

```joy
midi-virtual

\ Play middle C (pitch 60, velocity 80, duration 500ms)
60 80 500 midi-note

\ Play a C major chord
60 major 80 500 midi-chord

\ Using pitch names
"C4" pitch 80 500 midi-note
"E4" pitch 80 500 midi-note
"G4" pitch 80 500 midi-note
```

## Language Basics

Joy is a concatenative language where programs are built by composing functions. Data flows through a stack, and functions consume and produce stack values.

### Stack Operations

```joy
1 2 3       \ Push 1, 2, 3 onto stack: [1 2 3]
dup         \ Duplicate top: [1 2 3 3]
swap        \ Swap top two: [1 2 3 3] -> [1 2 3 3]
pop         \ Remove top: [1 2 3]
over        \ Copy second to top
rot         \ Rotate top three
```

### Arithmetic

```joy
3 4 + .     \ 7
10 3 - .    \ 7
6 7 * .     \ 42
20 4 / .    \ 5
17 5 mod .  \ 2
```

### Lists and Quotations

```joy
[1 2 3]           \ A list
[1 2 3] first .   \ 1
[1 2 3] rest .    \ [2 3]
[1 2 3] size .    \ 3

\ Quotations are executable code blocks
[dup *] 5 swap i .  \ 25 (squares 5)
```

### Combinators

```joy
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

### Control Messages

| Word | Stack Effect | Description |
|------|--------------|-------------|
| `midi-cc` | `( cc val -- )` | Send control change |
| `midi-program` | `( prog -- )` | Send program change |
| `midi-panic` | `( -- )` | All notes off (all channels) |

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

### Simple Melody with Notation

```joy
120 tempo
mf

\ Notes push pitches, play consumes them
c play d play e play f play
g play a play b play >> c play
```

### Chord Progression

```joy
\ Named chords push pitch lists
c:maj chord     \ I
f:maj chord     \ IV
g:maj chord     \ V
c:maj chord     \ I
```

### Using Joy Combinators

```joy
\ Transpose a melody using map
[c d e f g] i stack [7 +] map play

\ Generate chord sequence
[c d e f] i [[0] dip major] map
\ Now have list of chord lists
```

### Verbose API

```joy
midi-virtual

\ Play C major scale with explicit control
60 80 250 midi-note
62 80 250 midi-note
64 80 250 midi-note
65 80 250 midi-note
67 80 250 midi-note
69 80 250 midi-note
71 80 250 midi-note
72 80 500 midi-note

\ Chord progression with explicit control
60 major 80 500 midi-chord
65 major 80 500 midi-chord
67 major 80 500 midi-chord
60 major 80 1000 midi-chord
```

## Architecture

Joy-MIDI is built on the pyjoy-runtime, a clean C implementation of the Joy language:

```
projects/joy-midi/
  main.c              - Entry point, REPL setup
  joy_midi.c/h        - Primitive registration
  midi_primitives.c/h - MIDI implementations
  music_notation.c/h  - Alda-like notation parser
  music_context.c/h   - Musical state (octave, tempo, etc.)

thirdparty/pyjoy-runtime/
  joy_runtime.c/h     - Core Joy runtime
  joy_primitives.c    - Standard Joy primitives (~200 words)
  joy_parser.c/h      - Tokenizer and parser
```

### Design Philosophy

Notes in Joy-MIDI push MIDI pitch integers onto the stack rather than playing immediately. This enables Joy's compositional model:

```joy
\ Notes are data - can be transformed before playing
[c d e] i       \ Execute quotation, push 60 62 64
[7 +] map       \ Transpose up a fifth: 67 69 71
stack play      \ Collect and play
```

This design separates *what* to play (data) from *when* to play (action), allowing full use of Joy's combinators for musical transformations.

## See Also

- [Next Steps](next-steps.md) - Design ideas for more expressive musical primitives
- [Joy Language](https://hypercubed.github.io/joy/html/j00rat.html) - Original Joy documentation
- [Alda](https://alda.io/) - Music composition language that inspires Joy-MIDI's notation
