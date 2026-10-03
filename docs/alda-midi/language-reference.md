# Alda Language Reference

This document covers the Alda language features implemented in alda-midi.

For comprehensive Alda documentation, see the [alda-language](alda-language/) directory.

## Notes

Notes are specified by letter name (a-g), optional accidental, and optional duration.

```alda
c d e f g a b      # Natural notes in current octave
```

### Accidentals

```alda
c+ d+ f+ g+ a+     # Sharps (extension: cs ds fs gs as)
d- e- g- a- b-     # Flats (extension: db eb gb ab)
c++ d++            # Double sharps
d-- e--            # Double flats
c_                 # Natural (explicit)
```

`#` always starts a comment, as in Alda; `c#` is `c` followed by a comment.
The `s` and `b` suffixes are not Alda syntax.

```alda
```

### Durations

Durations are specified as note values (denominators):

| Value | Name | Ticks |
| ----- | ---- | ----- |
| 1 | Whole note | 1920 |
| 2 | Half note | 960 |
| 4 | Quarter note | 480 |
| 8 | Eighth note | 240 |
| 16 | Sixteenth note | 120 |
| 32 | Thirty-second | 60 |

```alda
c4                 # Quarter note
c8                 # Eighth note
c16                # Sixteenth note
c2                 # Half note
c1                 # Whole note
```

### Dotted Notes

Dots extend the duration by half:

```alda
c4.                # Dotted quarter (480 + 240 = 720 ticks)
c4..               # Double-dotted quarter (480 + 240 + 120 = 840 ticks)
c2.                # Dotted half
```

### Tied Notes

Ties connect two notes of the same pitch:

```alda
c4~4               # Quarter tied to quarter (held for 2 beats)
c2~8               # Half tied to eighth
c1~1               # Two whole notes tied
```

### Default Duration

If no duration is specified, the previous duration is used:

```alda
c4 d e f           # All quarter notes
c8 d e f g a b c   # All eighth notes
```

---

## Rests

Rests indicate silence:

```alda
r4                 # Quarter rest
r8                 # Eighth rest
r2.                # Dotted half rest
r1                 # Whole rest
```

---

## Octaves

### Setting Octave

```alda
o4 c d e           # Set octave to 4 (middle C = c in octave 4)
o5 c               # Octave 5
o3 c               # Octave 3
```

### Octave Shifts

```alda
> c                # Shift up one octave, then play c
< c                # Shift down one octave, then play c
>> c               # Shift up two octaves
```

### Octave Range

Octaves range from 0 to 9. Middle C (MIDI note 60) is `o4 c` or `c4` in octave 4.

| Octave | C MIDI Number |
| ------ | ------------- |
| 0 | 12 |
| 1 | 24 |
| 2 | 36 |
| 3 | 48 |
| 4 | 60 (Middle C) |
| 5 | 72 |
| 6 | 84 |
| 7 | 96 |
| 8 | 108 |

---

## Chords

Chords are simultaneous notes separated by `/`:

```alda
c/e/g              # C major triad
c/e/g/b            # C major 7th
c4/e/g             # Quarter note chord
d/f#/a             # D major
```

All notes in a chord share the same duration and start time.

---

## Parts (Instruments)

Parts declare which instrument plays the following events.

### Basic Part Declaration

```alda
piano:
  c d e f g

violin:
  c d e f g
```

### Part Groups

Apply events to multiple parts:

```alda
piano/violin:
  c d e f g        # Both instruments play the same notes
```

### Supported Instruments

All 128 General MIDI instruments are supported, including:

**Piano**: piano, acoustic-grand-piano, bright-acoustic-piano, electric-grand-piano, honky-tonk-piano, electric-piano-1, electric-piano-2, harpsichord, clavinet

**Chromatic Percussion**: celesta, glockenspiel, music-box, vibraphone, marimba, xylophone, tubular-bells, dulcimer

**Organ**: drawbar-organ, percussive-organ, rock-organ, church-organ, reed-organ, accordion, harmonica, tango-accordion

**Guitar**: acoustic-guitar-nylon, acoustic-guitar-steel, electric-guitar-jazz, electric-guitar-clean, electric-guitar-muted, overdriven-guitar, distortion-guitar, guitar-harmonics

**Bass**: acoustic-bass, electric-bass-finger, electric-bass-pick, fretless-bass, slap-bass-1, slap-bass-2, synth-bass-1, synth-bass-2

**Strings**: violin, viola, cello, contrabass, tremolo-strings, pizzicato-strings, orchestral-harp, timpani

**Ensemble**: string-ensemble-1, string-ensemble-2, synth-strings-1, synth-strings-2, choir-aahs, voice-oohs, synth-voice, orchestra-hit

**Brass**: trumpet, trombone, tuba, muted-trumpet, french-horn, brass-section, synth-brass-1, synth-brass-2

**Reed**: soprano-sax, alto-sax, tenor-sax, baritone-sax, oboe, english-horn, bassoon, clarinet

**Pipe**: piccolo, flute, recorder, pan-flute, blown-bottle, shakuhachi, whistle, ocarina

**Synth Lead**: lead-1-square, lead-2-sawtooth, lead-3-calliope, lead-4-chiff, lead-5-charang, lead-6-voice, lead-7-fifths, lead-8-bass-lead

**Synth Pad**: pad-1-new-age, pad-2-warm, pad-3-polysynth, pad-4-choir, pad-5-bowed, pad-6-metallic, pad-7-halo, pad-8-sweep

**Synth Effects**: fx-1-rain, fx-2-soundtrack, fx-3-crystal, fx-4-atmosphere, fx-5-brightness, fx-6-goblins, fx-7-echoes, fx-8-sci-fi

**Ethnic**: sitar, banjo, shamisen, koto, kalimba, bagpipe, fiddle, shanai

**Percussive**: tinkle-bell, agogo, steel-drums, woodblock, taiko-drum, melodic-tom, synth-drum, reverse-cymbal

**Sound Effects**: guitar-fret-noise, breath-noise, seashore, bird-tweet, telephone-ring, helicopter, applause, gunshot

See [list-of-instruments.md](alda-language/list-of-instruments.md) for the complete list.

---

## Attributes

Attributes modify playback characteristics using S-expression syntax.

### Tempo

```alda
(tempo 120)        # Set tempo to 120 BPM
(tempo 60)         # Slower
(tempo 180)        # Faster
```

### Volume

```alda
(volume 100)       # Full volume (0-100)
(volume 50)        # Half volume
(vol 80)           # Shorthand
(track-volume 50)  # MIDI CC 11, 0-100 percent (default CC 11 = 100)
```

### Dynamics

Dynamics set velocity/volume using musical terms. Velocities follow Alda
(`pp` 40, `p` 49, `mp` 59, `mf` 69, the default):

```alda
(ppp)              # Pianississimo (very very soft)
(pp)               # Pianissimo (very soft)
(p)                # Piano (soft)
(mp)               # Mezzo-piano (moderately soft)
(mf)               # Mezzo-forte (moderately loud)
(f)                # Forte (loud)
(ff)               # Fortissimo (very loud)
(fff)              # Fortississimo (very very loud)
```

### Quantization

Quantization controls note articulation (percentage of duration actually sounded):

```alda
(quant 120)        # Overlapping (values above 100 are allowed)
(quant 100)        # Legato (full duration)
(quant 90)         # Default (slightly detached)
(quant 50)         # Staccato (half duration)
(quantization 80)  # Full name
```

### Panning

```alda
(panning 0)        # Hard left
(panning 64)       # Center (default)
(panning 127)      # Hard right
(pan 32)           # Left of center
```

### Global Attributes and Channels

```alda
(tempo! 90)        # Tempo for every part; overrides the first part's (tempo N)
(key-sig! '(g major))  # Key signature for every part, including later ones
(midi-channel 3)   # Pin the part to MIDI channel 3 (0-15)
```

A part's own `(tempo N)` changes only that part's timing. The MIDI tempo map
follows the first part's tempo changes, as in Alda.

---

## Voices

Voices enable polyphonic writing within a single part.

### Basic Voices

```alda
piano:
  V1: c1 d e f     # Voice 1
  V2: e1 f g a     # Voice 2 (plays simultaneously)
```

### Voice Merging

`V0:` merges all voices, continuing from the latest position:

```alda
piano:
  V1: c4 d e f     # 4 quarter notes
  V2: c2 d         # 2 half notes (same total duration)
  V0: c1           # Continues after both voices finish
```

### Voice Numbers

Voices are numbered 1-8. Each voice has independent timing within the part.

---

## Repeats

Repeat sections of music:

```alda
[ c d e f ] * 4    # Repeat 4 times
c d e f * 2        # Repeat just the last note
```

---

## Barlines

Barlines are optional and ignored (for readability only):

```alda
c4 d e f | g a b c | d1
```

---

## Comments

```alda
# This is a comment
c d e f    # Inline comment
```

---

## MIDI Channel Assignment

Channels are assigned as notes need them. Each note's channel receives the
program, pan (CC 10) and track volume (CC 11) the note needs, when they differ
from what the channel holds. With more than 15 parts, a channel passes to
another part once its part stops sounding. `(midi-channel N)` pins a part.

Channel 10 is reserved for percussion in General MIDI.

---

## Implementation Status

Output matches Alda 2.4.7 note for note for the 40 scores in
`docs/alda-midi/examples/` and the 20 in `docs/alda-midi/shared_suite/`. See
`docs/alda-midi/conformance.md`.

---

## File Format

Alda files use the `.alda` extension and are plain text UTF-8.

```alda
# song.alda - Example Alda file

piano:
  (tempo 120)
  (mf)
  o4 c4 d e f | g2 g | a4 a a a | g2. r4
  f4 f f f | e2 e | d4 d d d | c1
```

Run with:

```bash
./build/alda_midi song.alda
```
