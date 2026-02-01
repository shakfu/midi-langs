# Wisp Syntax Guide

guile-midi supports [Wisp](https://www.draketo.de/software/wisp) (Whitespace to Lisp), an indentation-based syntax for Scheme. Wisp lets you write Scheme code without most parentheses, using indentation like Python.

## Quick Start

Run a wisp file:

```bash
./build/guile_midi song.w
```

## Syntax Overview

### Basic Function Calls

**Scheme:**
```scheme
(midi-note m c4 mf quarter)
(midi-chord m (major c4) f half)
```

**Wisp:**
```wisp
midi-note m c4 mf quarter
midi-chord m (major c4) f half
```

In Wisp, a line without leading whitespace becomes `(line-contents)`.

### Nested Calls with `:`

The colon `:` starts an inline function call:

**Scheme:**
```scheme
(display (major c4))
(define m (midi-open))
```

**Wisp:**
```wisp
display : major c4
define m : midi-open
```

### Multi-line Expressions

Use indentation for nested expressions:

**Scheme:**
```scheme
(for-each
  (lambda (p)
    (midi-note m p mf quarter))
  (scale c4 'major))
```

**Wisp:**
```wisp
for-each
  lambda (p)
    midi-note m p mf quarter
  scale c4 'major
```

### Continuing Arguments with `.`

Use `.` to continue arguments on the next line:

**Scheme:**
```scheme
(midi-chord m (major c4) f half)
```

**Wisp:**
```wisp
midi-chord m : major c4
  . f half
```

### Definitions

**Scheme:**
```scheme
(define (play-scale root)
  (for-each
    (lambda (p) (midi-note *midi* p mf eighth))
    (scale root 'major)))
```

**Wisp:**
```wisp
define : play-scale root
  for-each
    lambda (p)
      midi-note *midi* p mf eighth
    scale root 'major
```

## Complete Example

Here's a complete song in Wisp syntax:

**song.w:**
```wisp
; Simple melody in Wisp syntax

define m : midi-open
set-tempo! 120

; Play C major scale
for-each
  lambda (p)
    midi-note m p mf eighth
  scale c4 'major

; Rest
midi-sleep 500

; Chord progression: I - IV - V - I
midi-chord m : major c4
  . mf half
midi-chord m : major f3
  . mf half
midi-chord m : major g3
  . f half
midi-chord m : major c4
  . mf whole

midi-close m
```

Run with:

```bash
./build/guile_midi song.w
```

## Async Voices in Wisp

**Scheme:**
```scheme
(open)
(spawn
  (make-melody-voice (scale c4 'pentatonic) mf quarter)
  "melody")
(spawn
  (make-repeat-voice
    (lambda () (midi-note *midi* c2 f sixteenth))
    8
    eighth)
  "bass")
(run)
(close)
```

**Wisp:**
```wisp
open

spawn
  make-melody-voice : scale c4 'pentatonic
    . mf quarter
  . "melody"

spawn
  make-repeat-voice
    lambda ()
      midi-note *midi* c2 f sixteenth
    . 8 eighth
  . "bass"

run
close
```

## Wisp Syntax Rules

| Wisp | Scheme | Description |
|------|--------|-------------|
| `foo bar baz` | `(foo bar baz)` | Line becomes a list |
| `foo : bar baz` | `(foo (bar baz))` | `:` starts nested call |
| Indented lines | Nested in parent | Child expressions |
| `. arg1 arg2` | Continues previous | Additional arguments |
| `; comment` | `; comment` | Comments unchanged |
| `'symbol` | `'symbol` | Quotes work normally |

## Mixing Wisp and Scheme

You can use parentheses anywhere in Wisp for clarity or complex expressions:

```wisp
; Parentheses work normally
define chord (list c4 e4 g4)

; Mix freely
midi-chord m (major c4) mf half

; Complex expressions may be clearer with parens
define pitches
  map (lambda (x) (+ x 12)) : scale c4 'major
```

## File Extensions

guile-midi recognizes these extensions:

- `.w` - Wisp syntax
- `.wisp` - Wisp syntax
- `.scm` - Standard Scheme

## Loading Wisp from Scheme

You can also load wisp programmatically:

```scheme
(use-modules (language wisp))

;; Read wisp string
(define exprs (wisp-scheme-read-string "
define m : midi-open
midi-note m c4 mf quarter
midi-close m
"))

;; Evaluate
(for-each (lambda (e) (eval e (current-module))) exprs)

;; Or load a file
(for-each
  (lambda (e) (eval e (current-module)))
  (wisp-scheme-read-file "song.w"))
```

## Why Wisp?

Wisp offers several advantages:

- **Familiar indentation**: Python/YAML-like structure
- **Less punctuation**: Fewer parentheses to balance
- **Still Scheme**: Full language power, just different surface syntax
- **Readable**: Clear visual structure for nested expressions
- **Portable**: Wisp files can be converted to standard Scheme

## Limitations

- Wisp requires consistent indentation (spaces recommended)
- Some complex expressions may be clearer in standard Scheme
- Error messages reference the translated Scheme, not original Wisp

## Resources

- [Wisp Homepage](https://www.draketo.de/software/wisp)
- [Wisp Specification](https://srfi.schemers.org/srfi-119/srfi-119.html) (SRFI-119)
