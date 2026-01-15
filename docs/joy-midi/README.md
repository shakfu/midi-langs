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
Type 'quit' to exit, 'midi-list' to list MIDI ports
> 3 4 + .
7
> "C4" pitch .
60
> 60 major .
[60 64 67]
```

### Playing Notes (Alda-like Notation)

Joy-MIDI supports concise Alda-like musical notation:

```joy
\ Create a virtual MIDI port and play a scale
midi-virtual
o4 c d e f g a b >> c

\ Duration suffixes (4=quarter, 8=eighth, etc.)
c4 d8 e8 f4 g2

\ Accidentals (+ sharp, - flat)
c c+ d d- e f+ g

\ Chords (slash notation)
c/e/g d/f/a e/g/b

\ Named chords
c:maj d:min e:min f:maj g:7

\ Dynamics
mf c d e f
ff g a b >> c

\ Tempo and quantization
120 tempo 50 quant
c d e f
```

### Playing Notes (Verbose)

For more control, use explicit MIDI primitives:

```joy
\ Create a virtual MIDI port
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

### Simple Melody

```joy
midi-virtual

\ Play C major scale
60 80 250 midi-note
62 80 250 midi-note
64 80 250 midi-note
65 80 250 midi-note
67 80 250 midi-note
69 80 250 midi-note
71 80 250 midi-note
72 80 500 midi-note
```

### Chord Progression

```joy
midi-virtual

\ I-IV-V-I in C major
60 major 80 500 midi-chord
65 major 80 500 midi-chord
67 major 80 500 midi-chord
60 major 80 1000 midi-chord
```

### Using Transpose

```joy
midi-virtual

\ Play a pattern, then transpose up a fifth
60 80 250 midi-note
64 80 250 midi-note
67 80 500 midi-note

\ Same pattern, transposed up 7 semitones
60 7 transpose 80 250 midi-note
64 7 transpose 80 250 midi-note
67 7 transpose 80 500 midi-note
```

## Architecture

Joy-MIDI is built on the pyjoy-runtime, a clean C implementation of the Joy language:

```
projects/joy-midi/
  main.c              - Entry point, REPL setup
  joy_midi.c/h        - Primitive registration
  midi_primitives.c/h - MIDI implementations

thirdparty/pyjoy-runtime/
  joy_runtime.c/h     - Core Joy runtime
  joy_primitives.c    - Standard Joy primitives (~200 words)
  joy_parser.c/h      - Tokenizer and parser
```

## See Also

- [Next Steps](next-steps.md) - Design ideas for more expressive musical primitives
- [Joy Language](https://hypercubed.github.io/joy/html/j00rat.html) - Original Joy documentation
- [Alda](https://alda.io/) - Music composition language that inspires future Joy-MIDI features
