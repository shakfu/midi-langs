---
layout: page
title: Cheat sheet
subtitle: Alda at a glance
permalink: /cheat-sheet/
---

_If you're trying these examples yourself and you don't hear anything, make sure
that you specify an instrument. For example:_

```alda
piano: c d e f g
```

_Once you declare an instrument in a REPL session, that instrument will be
"active" so that you don't have to keep specifying it._

---

**[Comments](#comments) •
[Notes](#notes) •
[Rests](#rests) •
[Octaves](#octaves) •
[Accidentals](#accidentals) •
[Note lengths / duration](#note-lengths--duration) •
[Ties](#ties) •
[Slurs / legato](#slurs--legato) •
[Cram expressions](#cram-expressions) •
[Chords](#chords) •
[Voices](#voices) •
[Parts](#parts) •
[Tempo](#tempo) •
[Volume / dynamics](#volume--dynamics) •
[Panning](#panning) •
[Quantization (legato <-> staccato)](#quantization-legato---staccato) •
[Key signature](#key-signature) •
[Markers](#markers) •
[Variables](#variables) •
[Repeats](#repeats)**

---

## Comments

```alda
# This is a comment.
piano: c   # This is also a comment.
```

## Notes

```alda
c d e f g
```

## Rests

```alda
c r d r e r f r g
```

## Octaves

Explicit octave numbers:

```alda
o0 c o1 c o2 c o3 c o4 c o5 c o6 c o7 c o8 c o9 c

o4 g a b o5 c o4 b a g
```

Octave up (`>`) and down (`<`):

```alda
o0 c > c > c > c > c > c > c > c > c > c

o4 g a b > c < b a g
```

## Accidentals

Sharps (`+`) and flats (`-`):

```alda
c < b- a g f+
```

Double flats/sharps:

```alda
g+ f+ e+ d+ c++
```

The sky is the limit!:

```alda
c++++-+-+-+
```

## Note lengths / duration

Standard "powers of two" note lengths (quarter, eighth, etc.):

```alda
c4 c8 c c16 c c c c32 c c c c c c c | c1
```

Dotted notes:

```alda
c4 c4
c4. c8
c4.. c16
c4... c32
c2. c4
```

Non-standard note lengths:

```alda
c9 c8 c7 c6 c5 c4 c3 c2 c1
```

Second (`s`) and millisecond (`ms`) note durations:

```alda
c2s d522ms e1234ms f5s
```

## Ties

```alda
g+1~1

c2.~500ms~4..
```

## Slurs / legato

```alda
c4~ d~ e~ f
```

## Cram expressions

```alda
{c}2
{c d}2
{c d e}2
{c d e f}2
{c d e f g}2
{c d e f g a}2
{c d e f g a}4 {c d e f g a}4 {d e f g a b}4 > c2
```

Nested cram expressions:

```alda
{c {d {d+ e} f f+} g a}1
```

## Chords

```alda
c1/e-/g/b

c1/e/g/>c4 < b a g | < g+1/b/>e
```

## Voices

```alda
V1: c8 d e f g a b > c1
V2: e8 f g a b > c d e1
V3: g8 a b > c d e f g1
V4: o3 c2 g4 < g8 c1
```

## Parts

```alda
oboe:
  c8 d e f g a b > c1

clarinet:
  e8 f g a b > c d e1

flute:
  g8 a b > c d e f g1

bassoon:
  o3 c2 g4 < g8 c1
```

Named parts:

```alda
cello "my-cello-1":
  o2 c1~1

cello "my-cello-2":
  r2 o2 g2~1

cello "my-cello-3":
  r1 o3 e1

my-cello-1:
  d2 e f1

my-cello-2:
  a2 b > c1

my-cello-3:
  f2 g a1
```

Part groups:

```alda
violin/viola/cello "strings":
  g1

strings:
  e

strings.violin:
  > c

strings.viola:
  e

strings.cello:
  < c
```

## Tempo

**Local** tempo (only affects the current part):

```alda
trumpet:
  (tempo 200) o4 c8 d e f g a b > c4.

trombone:
  o3 e8 f g a b > c d e4.
```

**Global** tempo (affects all parts):

```alda
trumpet:
  (tempo! 200) o4 c8 d e f g a b > c4.

trombone:
  o3 e8 f g a b > c d e4.
```

You can also declare a global tempo at the top of the score:

```alda
(tempo! 150)

clarinet:
  c d e

flute:
  e f g
```

## Volume / dynamics

Dynamic markings:

```alda
(pppppp) c
(ppppp) c
(pppp) c
(ppp) c
(pp) c
(p) c
(mp) c
(mf) c
(f) c
(ff) c
(fff) c
(ffff) c
(fffff) c
(ffffff) c
```

Explicit volume level (0-100):

```alda
(vol 20) c
(vol 35) c
(vol 57) c
(vol 75) c
(vol 89) c
(vol 100) c
```

## Panning

```alda
piano:
  (panning 0)   c8
  (panning 15)  d
  (panning 30)  e
  (panning 45)  f
  (panning 60)  g
  (panning 75)  a
  (panning 90)  b
  (panning 100) > c

  V1: (panning 0)
    o2 g2/>c/e < a-/>c/f < g1/>c/e

  V2: (panning 100)
    o4 r4 c2/e/g c4/f/a- c1/e/g
```

## Quantization (legato <-> staccato)

```alda
bassoon:
  o2 d8 e (quant 30) f+ g (quant 99) a2
```

## Key signature

Major and minor keys:

```alda
(key-sig! '(d major))
o4 d8 e f g a b > c d

(key-sig! '(d minor))
o4 d8 e f g a b > c d
```

Sharp/flat keys:

```alda
(key-sig! '(b flat major))
o3 b8 > c d e f g a b

(key-sig! '(c sharp major))
o4 c8 d e f g a b > c
```

Modes:

```alda
(key-sig! '(d ionian))
o4 d8 e f g a b > c d

(key-sig! '(d dorian))
o4 d8 e f g a b > c d

(key-sig! '(d phrygian))
o4 d8 e f g a b > c d

(key-sig! '(d lydian))
o4 d8 e f g a b > c d

(key-sig! '(d mixolydian))
o4 d8 e f g a b > c d

(key-sig! '(d aeolian))
o4 d8 e f g a b > c d

(key-sig! '(d locrian))
o4 d8 e f g a b > c d
```

Custom key signatures:

```alda
(key-sig! "f+ b- d+")
o4 c8 d e f g a b > c

(key-sig! '(f (sharp) b (flat) d (sharp)))
o4 c8 d e f g a b > c
```

## Markers

```alda
trumpet:
  o4 c8 d e f g a b > %last-note c4.~2

trombone:
  o3 e8 f g a b > c d e4.~2

tuba:
  @last-note o2 c4.~2
```

## Variables

Define riffs/motifs:

```alda
(tempo! 200)

riff = b4 b8 > d4 d8 e4 <

electric-bass:
  o1 riff *4

electric-guitar-distorted:
  o2 riff *4
```

Define your own shorthand:

```alda
quiet  = (vol 25)
loud   = (vol 50)
louder = (vol 75)

notes  = c d e

piano:
  quiet notes
  loud notes
  louder notes
```

Multi-line variable definition:

```alda
(key-sig! '(a flat major))
(tempo! 90)

cocoon = [
  a4 > c8 e g8. e g8 <
  a8. > c e8~2 <
]

midi-synth-pad-new-age:
  (quant 95)
  o2 cocoon *2
```

## Repeats

Repeat a note:

```alda
c *4
```

Repeat a chord:

```alda
c/e/g *4
```

Repeat a sequence:

```alda
[c16 d e f]*3 g2
```

Repeat a variable:

```alda
run = c16 d e f
run*3 g2
```

Repeat with variations:

```alda
[
  o4
  c16 '1,4
  d16 '2,5
  e16 '3,6
  f16 '7-8
  g16 '9

  o3 c16 c c
]*9

o4 a1
```
