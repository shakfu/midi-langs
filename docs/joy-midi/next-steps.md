# Joy-MIDI: Next Steps

This document outlines design ideas for making Joy-MIDI more expressive, drawing inspiration from Alda's musical notation while staying true to Joy's concatenative paradigm.

## Design Goal

Make musical notation as concise as Alda while preserving Joy's compositional power. In Alda, `c d e f g` plays five notes. Joy-MIDI should be equally terse.

## Current vs. Proposed

```joy
\ Current (verbose)
midi-virtual
"C4" pitch 80 500 midi-note
"D4" pitch 80 500 midi-note
"E4" pitch 80 500 midi-note

\ Proposed (concise, Alda-like)
midi-virtual
c d e
```

## Core Notation Design

### Notes as Words

Each note letter becomes a word that plays immediately using current state:

```joy
c d e f g a b    \ Play C D E F G A B (current octave, duration, velocity)
```

### Duration Suffixes

Following Alda, duration is a numeric suffix:

```joy
c1        \ Whole note C
c2        \ Half note C
c4        \ Quarter note C (default)
c8        \ Eighth note C
c16       \ Sixteenth note C
c32       \ Thirty-second note C

c4 d e f  \ Quarter C, then D E F inherit quarter duration
c8 d e f  \ Eighth C, then D E F are eighths
```

### Dotted Notes

```joy
c4.       \ Dotted quarter (1.5x duration)
c4..      \ Double-dotted quarter (1.75x)
c2.       \ Dotted half
```

### Accidentals

```joy
c+ d e f+ g     \ C sharp, D, E, F sharp, G
c- d e- f g     \ C flat, D, E flat, F, G
c++ d--         \ Double sharp, double flat
c_              \ Natural (override key signature)
```

### Octaves

```joy
o4 c d e        \ Set octave 4, play C D E
> c             \ Octave up, then C
< c             \ Octave down, then C
o4 g a b > c    \ G A B in octave 4, C in octave 5
```

### Rests

```joy
c4 r d r e      \ C, rest, D, rest, E (all quarters)
c4 r8 r d       \ Quarter C, two eighth rests, quarter D
r1              \ Whole rest
```

### Ties

```joy
c1~1            \ C whole note tied to another whole
c4~8            \ Quarter tied to eighth
c2.~4           \ Dotted half tied to quarter
```

## Chords

### Slash Notation (Alda-style)

```joy
c/e/g           \ C major triad (simultaneous)
c1/e/g          \ Whole note C major chord
c/e-/g/b        \ C minor 7th chord
```

### Named Chords

```joy
c:maj           \ C major triad
c:min           \ C minor triad
c:dim           \ C diminished
c:aug           \ C augmented
c:7             \ C dominant 7th
c:maj7          \ C major 7th
c:min7          \ C minor 7th
c:dim7          \ C diminished 7th

c4:maj d:min e:min f:maj   \ Chord progression
```

### Chord Inversions

```joy
c:maj           \ Root position [C E G]
c:maj/1         \ First inversion [E G C]
c:maj/2         \ Second inversion [G C E]
```

## Tuplets / Cram Expressions

Fit N notes into a duration (Alda's `{...}` syntax):

```joy
{c d e}2        \ Triplet: 3 notes in half note duration
{c d e f}4      \ 4 notes in quarter note duration
{c d}8          \ 2 notes in eighth note duration

\ Nested crams
{c {d e} f}2    \ Complex rhythmic grouping
```

## Voices (Polyphony)

```joy
V1: c8 d e f g a b > c1
V2: e8 f g a b > c d e1
V3: g8 a b > c d e f g1
V0:             \ End voices, merge

\ After V0:, playback continues from longest voice endpoint
```

## Repeats

```joy
c*4             \ Repeat C four times
c/e/g*4         \ Repeat chord four times
[c d e f]*3     \ Repeat sequence three times

\ Variables for motifs
riff = c8 d e f
riff*4          \ Play riff four times
```

### Repeat with Variations

```joy
[
  c8 '1,3       \ Play on repetitions 1 and 3
  d8 '2,4       \ Play on repetitions 2 and 4
  e8            \ Play every time
]*4
```

## Attributes

### Tempo

```joy
(tempo 120)     \ Set local tempo to 120 BPM
(tempo! 120)    \ Set global tempo (all parts)

120 tempo       \ Alternative stack-based syntax
```

### Dynamics

```joy
(ppp) c d e     \ Very soft
(pp) c d e
(p) c d e       \ Soft
(mp) c d e      \ Medium soft
(mf) c d e      \ Medium loud (default)
(f) c d e       \ Loud
(ff) c d e
(fff) c d e     \ Very loud

(vol 75) c d e  \ Explicit volume (0-100)
```

### Quantization (Legato/Staccato)

```joy
(quant 90) c d e    \ 90% duration (legato, default)
(quant 50) c d e    \ 50% duration (staccato)
(quant 100) c d e   \ Full duration (very legato)
```

### Panning

```joy
(pan 0) c           \ Hard left
(pan 50) c          \ Center (default)
(pan 100) c         \ Hard right
```

### Key Signature

```joy
(key c:maj) c d e f g a b > c    \ C major (no accidentals)
(key g:maj) c d e f g a b > c    \ G major (F is sharp)
(key d:min) c d e f g a b > c    \ D minor

\ Notes automatically use key signature accidentals
\ Use natural (_) to override
(key g:maj) f       \ Plays F#
(key g:maj) f_      \ Plays F natural
```

## Parts / Instruments

```joy
piano:
  c8 d e f g a b > c4

trumpet:
  o5 c4 d e f g2

violin/viola/cello "strings":
  o3 c1/e/g
```

## Markers and Variables

```joy
\ Define a variable (motif)
melody = c4 d e8 e f4 g

\ Use it
melody
melody [7 +] map    \ Transpose up a fifth (Joy-style transformation)

\ Markers for synchronization
%chorus c d e f
@chorus g a b > c   \ Jump to chorus marker
```

## Time Signatures (Display/Structure)

```joy
(time 4/4)          \ Common time
(time 3/4)          \ Waltz time
(time 6/8)          \ Compound duple

|                   \ Barline (visual, no effect on playback)
```

## Complete Example

```joy
\ "Ode to Joy" theme in Joy-MIDI

midi-virtual
(tempo! 120)
(key d:maj)

piano:
  o4
  (mf)

  \ First phrase
  f+4 f+ g a | a g f+ e | d d e f+ | f+4. e8 e2 |

  \ Second phrase
  f+4 f+ g a | a g f+ e | d d e f+ | e4. d8 d2 |

  \ Bridge
  e4 e f+ d | e f+8 g f+4 d | e f+8 g f+4 e | d e < a4 > |

  \ Return
  (f)
  f+4 f+ g a | a g f+ e | d d e f+ | e4. d8 d2
```

## Implementation Phases

### Phase 1: Core Notation
1. Note words (`c` through `b`) with duration state
2. Duration suffixes (`c4`, `c8`, etc.)
3. Dotted notes (`c4.`, `c4..`)
4. Accidentals (`c+`, `c-`, `c_`)
5. Octave control (`o4`, `>`, `<`)
6. Rests (`r`, `r4`, etc.)

### Phase 2: Chords and Ties
1. Slash notation (`c/e/g`)
2. Named chords (`c:maj`, `c:min7`)
3. Ties (`c4~8`)
4. Inversions (`c:maj/1`)

### Phase 3: Rhythm and Structure
1. Cram expressions (`{c d e}4`)
2. Repeats (`c*4`, `[c d e]*3`)
3. Variables (`riff = c d e`)

### Phase 4: Expression
1. Dynamics (`(mf)`, `(vol 75)`)
2. Tempo (`(tempo 120)`)
3. Quantization (`(quant 50)`)
4. Key signatures (`(key g:maj)`)

### Phase 5: Polyphony
1. Voices (`V1:`, `V2:`, `V0:`)
2. Parts (`piano:`, `trumpet:`)
3. Part groups (`violin/viola/cello`)

## Alda to Joy-MIDI Mapping

| Alda | Joy-MIDI | Description |
|------|----------|-------------|
| `c` | `c` | Play C (default duration) |
| `c4` | `c4` | Quarter note C |
| `c8.` | `c8.` | Dotted eighth C |
| `c+` | `c+` | C sharp |
| `c-` | `c-` | C flat |
| `o4` | `o4` | Set octave 4 |
| `>` | `>` | Octave up |
| `<` | `<` | Octave down |
| `r` | `r` | Rest |
| `c/e/g` | `c/e/g` | C major chord |
| `c4~4` | `c4~4` | Tied quarters |
| `{c d e}4` | `{c d e}4` | Triplet |
| `c*4` | `c*4` | Repeat 4 times |
| `[c d e]*3` | `[c d e]*3` | Repeat sequence |
| `V1:` | `V1:` | Voice 1 |
| `(tempo 120)` | `(tempo 120)` | Set tempo |
| `(mf)` | `(mf)` | Mezzo-forte |
| `(key-sig '(g major))` | `(key g:maj)` | G major key |
| `piano:` | `piano:` | Piano part |
| `riff = c d e` | `riff = c d e` | Define variable |

## Joy-Specific Extensions

While closely following Alda syntax, Joy-MIDI can leverage Joy's unique features:

### Quotations for Transformation

```joy
[c d e f] 'phrase def      \ Define phrase
phrase                      \ Play it
phrase reverse              \ Play backwards
phrase [7 +] map            \ Transpose up a fifth
phrase [dup] map            \ Double each note (tremolo effect)
```

### Stack-Based Composition

```joy
c d e f                     \ Build melody on stack implicitly
[c d e f] [g a b > c] concat  \ Concatenate phrases
```

### Combinators for Variation

```joy
[c d e] 3 times             \ Repeat 3 times
[c4 d e f] [2 *] map        \ Double all durations (half-time)
[c d e f g] shuffle         \ Randomize order
```

### Conditional Playback

```joy
random 50 > [c] [e] ifte    \ 50% chance C or E
[c d e] [f g a] 0.7 choose  \ 70% first, 30% second
```

## References

- [Alda Language](https://alda.io/) - Primary inspiration
- [Alda Cheat Sheet](../alda-midi/alda-language/alda-overview.md) - Notation reference
- [Joy Language](https://hypercubed.github.io/joy/html/j00rat.html) - Concatenative programming
- [Forth MIDI](../../projects/forth-midi/) - Related stack-based MIDI implementation
