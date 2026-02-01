# guile-midi Examples

The guile-midi API is identical to s7-midi, so most examples work interchangeably. This document provides examples specifically tested with guile-midi.

## Basic Examples

### Hello MIDI

Play a C major scale:

```scheme
(define m (midi-open))

(for-each (lambda (p) (midi-note m p mf quarter))
          (list c4 d4 e4 f4 g4 a4 b4 c5))

(midi-close m)
```

### Using Note Names

Play notes using string notation:

```scheme
(define m (midi-open))

(midi-note m (note "C4"))
(midi-note m (note "D4"))
(midi-note m (note "E4"))
(midi-note m (note "F4"))
(midi-note m (note "G4") f half)  ; Louder, longer

(midi-close m)
```

### Chord Progression

Play a I-IV-V-I progression:

```scheme
(define m (midi-open))

;; I - C major
(midi-chord m (major c4) mf half)

;; IV - F major
(midi-chord m (major f3) mf half)

;; V - G major
(midi-chord m (major g3) mf half)

;; I - C major
(midi-chord m (major c4) f whole)

(midi-close m)
```

### Melody with Dynamics

```scheme
(define m (midi-open))

;; Crescendo
(midi-note m c4 pp quarter)
(midi-note m d4 p quarter)
(midi-note m e4 mp quarter)
(midi-note m f4 mf quarter)
(midi-note m g4 f quarter)
(midi-note m a4 ff half)

;; Decrescendo
(midi-note m g4 f quarter)
(midi-note m f4 mf quarter)
(midi-note m e4 mp quarter)
(midi-note m d4 p quarter)
(midi-note m c4 pp whole)

(midi-close m)
```

---

## Intermediate Examples

### Arpeggiated Chords

```scheme
(define m (midi-open))

;; Arpeggiate C major up and down
(midi-arpeggio m (list c4 e4 g4 c5) mf eighth)
(midi-arpeggio m (list c5 g4 e4 c4) mf eighth)

(rest quarter)

;; Arpeggiate A minor
(midi-arpeggio m (minor a4) ff sixteenth)
(midi-arpeggio m (reverse (minor a4)) ff sixteenth)

(midi-close m)
```

### Tempo Changes

```scheme
;; Start slow
(set-tempo! 60)

(define m (midi-open))

(midi-note m c4 mf quarter)  ; 1000ms at 60 BPM
(midi-note m e4 mf quarter)
(midi-note m g4 mf quarter)

;; Speed up
(set-tempo! 120)
(midi-note m c5 mf quarter)  ; 500ms at 120 BPM
(midi-note m e5 mf quarter)
(midi-note m g5 mf quarter)

;; Even faster
(set-tempo! 180)
(midi-note m c6 mf quarter)
(midi-note m e6 mf quarter)
(midi-note m g6 mf quarter)

(midi-close m)
```

### Dotted Rhythms

```scheme
(define m (midi-open))

;; Dotted quarter - eighth pattern
(midi-note m c4 mf (dotted quarter))
(midi-note m d4 mf eighth)
(midi-note m e4 mf (dotted quarter))
(midi-note m f4 mf eighth)
(midi-note m g4 f half)

(midi-close m)
```

---

## Scale Examples

### Playing Scales

```scheme
(define m (midi-open))

;; Play C major scale using helper
(for-each (lambda (p) (midi-note m p mf eighth))
          (scale c4 'major))

(rest quarter)

;; Play D dorian scale
(for-each (lambda (p) (midi-note m p mf eighth))
          (scale d4 'dorian))

(midi-close m)
```

### Modal Exploration

Play through all diatonic modes starting on C:

```scheme
(define m (midi-open))

(define modes '(major dorian phrygian lydian mixolydian minor locrian))

(for-each
  (lambda (mode)
    (display mode) (newline)
    (for-each (lambda (p) (midi-note m p mf sixteenth))
              (scale c4 mode))
    (rest quarter))
  modes)

(midi-close m)
```

### Using Scale Degrees

Build chords from scale degrees:

```scheme
(define m (midi-open))

;; Play I-IV-V-I in C major using scale degrees
(define root c4)

;; I chord (1, 3, 5)
(midi-chord m (list (degree root 'major 1)
                    (degree root 'major 3)
                    (degree root 'major 5)) mf half)

;; IV chord (4, 6, 8)
(midi-chord m (list (degree root 'major 4)
                    (degree root 'major 6)
                    (degree root 'major 8)) mf half)

;; V chord (5, 7, 9)
(midi-chord m (list (degree root 'major 5)
                    (degree root 'major 7)
                    (degree root 'major 9)) mf half)

;; I chord
(midi-chord m (list (degree root 'major 1)
                    (degree root 'major 3)
                    (degree root 'major 5)) f whole)

(midi-close m)
```

### Blues Scale Improvisation

```scheme
(define m (midi-open))
(set-tempo! 100)

(define blues (scale c4 'blues))

;; Add octave above for more range
(define full-blues
  (append blues (map (lambda (p) (+ p 12)) blues)))

;; Random blues licks
(do ((bar 0 (+ bar 1)))
    ((= bar 4))
  (do ((beat 0 (+ beat 1)))
      ((= beat 4))
    (let ((note-count (+ 1 (random 3))))
      (do ((n 0 (+ n 1)))
          ((= n note-count))
        (let ((pitch (list-ref full-blues (random (length full-blues))))
              (vel (+ 60 (random 40))))
          (midi-note m pitch vel sixteenth))))))

;; End on the root
(midi-note m c4 f whole)

(midi-close m)
```

---

## Async Playback Examples

### Multiple Concurrent Voices

```scheme
(open)

;; Bass voice - steady eighth notes
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* c2 f sixteenth))
         16
         eighth)
       "bass")

;; Lead melody - pentatonic scale
(spawn (make-melody-voice (scale c4 'pentatonic) mf quarter) "melody")

(run)  ; Both play simultaneously
(close)
```

### Polyrhythmic Pattern

Create 3 against 4 polyrhythm:

```scheme
(open)

;; 3 beats per cycle
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* 60 mf 100))
         3
         400)
       "3-beat")

;; 4 beats per cycle (different rate)
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* 67 mf 100))
         4
         300)
       "4-beat")

(run)
(close)
```

### Chord Progression with Bass

```scheme
(open)

;; Bass pattern (loops 4 times)
(spawn (make-repeat-voice
         (lambda ()
           (midi-note *midi* c2 f eighth)
           (midi-note *midi* g2 mf eighth))
         4
         quarter)
       "bass")

;; Chord progression
(spawn (make-sequence-voice
         (list
           (cons (lambda () (midi-chord *midi* (major c4) mf quarter)) quarter)
           (cons (lambda () (midi-chord *midi* (minor a3) mf quarter)) quarter)
           (cons (lambda () (midi-chord *midi* (major f3) mf quarter)) quarter)
           (cons (lambda () (midi-chord *midi* (major g3) f quarter)) 0)))
       "chords")

(run)
(close)
```

### Random Walker Voice

Create a voice that randomly walks through pitches:

```scheme
(open)

(define (make-random-walker)
  (let ((pitch 60)
        (steps 20))
    (lambda ()
      (if (<= steps 0)
          #f
          (begin
            (midi-note *midi* pitch mf sixteenth)
            (set! pitch (+ pitch (- (random 5) 2)))  ; random walk
            (set! pitch (max 48 (min 84 pitch)))    ; clamp range
            (set! steps (- steps 1))
            eighth)))))

(spawn (make-random-walker) "walker")
(run)
(close)
```

### Looping with Manual Stop

```scheme
(open)

;; Create a looping beat
(define beat-id
  (spawn (make-loop-voice
           (lambda ()
             (midi-note *midi* 36 f sixteenth)  ; Kick
             (midi-sleep 250)
             (midi-note *midi* 38 mf sixteenth)) ; Snare
           500)
         "beat"))

;; Let it run for a while, then stop
(midi-sleep 4000)
(stop beat-id)

(close)
```

---

## Advanced Examples

### Higher-Order Functions

Use Scheme's functional features for musical patterns:

```scheme
(define m (midi-open))

;; Create a note-playing function with fixed parameters
(define (make-player velocity duration)
  (lambda (pitch)
    (midi-note m pitch velocity duration)))

;; Define different "instruments"
(define loud-short (make-player fff sixteenth))
(define soft-long (make-player pp half))

;; Use them
(for-each loud-short (list c4 e4 g4 c5))
(for-each soft-long (list c5 g4 e4 c4))

(midi-close m)
```

### Pattern Repetition with times

```scheme
(define m (midi-open))

;; Define a pattern as a procedure
(define (pattern1)
  (midi-note m c4 mf eighth)
  (midi-note m e4 mf eighth)
  (midi-note m g4 mf eighth)
  (midi-note m e4 mf eighth))

;; Repeat it 4 times using times helper
(times 4 pattern1)

(midi-close m)
```

### Transpose a Melody

```scheme
(define m (midi-open))

(define melody (list c4 d4 e4 f4 g4))

;; Original
(for-each (lambda (p) (midi-note m p mf eighth)) melody)
(rest quarter)

;; Up a perfect fifth
(for-each (lambda (p) (midi-note m (transpose p 7) mf eighth)) melody)
(rest quarter)

;; Down an octave
(for-each (lambda (p) (midi-note m (octave-down p) mf eighth)) melody)

(midi-close m)
```

### Generative Music

```scheme
(define m (midi-open))
(set-tempo! 140)

;; Random note from a list
(define (random-element lst)
  (list-ref lst (random (length lst))))

;; Random velocity in range
(define (random-velocity min max)
  (+ min (random (- max min))))

;; Play 32 random notes from C major 7
(define chord (maj7 c4))
(do ((i 0 (+ i 1)))
    ((= i 32))
  (midi-note m
             (random-element chord)
             (random-velocity 60 100)
             sixteenth))

(midi-close m)
```

### Probability-Based Patterns

```scheme
(define m (midi-open))

;; Play note with probability
(define (maybe-play pitch prob)
  (if (< (random 100) prob)
      (midi-note m pitch mf sixteenth)
      (rest sixteenth)))

;; Sparse texture
(do ((i 0 (+ i 1)))
    ((= i 32))
  (maybe-play c4 30)   ; 30% chance
  (maybe-play e4 50)   ; 50% chance
  (maybe-play g4 70))  ; 70% chance

(midi-close m)
```

### Multi-Channel Composition

```scheme
(define m (midi-open))

;; Set up instruments
(midi-program m 0 1)    ; Piano on channel 1
(midi-program m 32 2)   ; Bass on channel 2

;; Helper for channel-specific playing
(define (play-on-channel ch pitch vel dur)
  (midi-note m pitch vel dur ch))

;; Simple bass line and melody
(define (bar)
  ;; Bass note (channel 2)
  (play-on-channel 2 c2 100 half)
  ;; Melody notes (channel 1)
  (play-on-channel 1 c4 80 quarter)
  (play-on-channel 1 e4 80 quarter))

;; Play 4 bars
(times 4 bar)

(midi-close m)
```

---

## Recording and File I/O

### Record and Save to Scheme

```scheme
(define m (midi-open))

(record-midi 120)

(midi-note m c4 mf quarter)
(midi-note m e4 mf quarter)
(midi-chord m (major g4) f half)

(record-stop)
(save-midi "melody.scm")

(midi-close m)
```

### Record and Export MIDI File

```scheme
(define m (midi-open))

(record-midi 120)

;; Play a progression
(midi-chord m (major c4) mf half)
(midi-chord m (minor a3) mf half)
(midi-chord m (major f3) mf half)
(midi-chord m (major g3) f half)
(midi-chord m (major c4) mf whole)

(record-stop)
(write-mid "progression.mid")

(midi-close m)
```

### Read MIDI File

```scheme
(define data (read-mid "song.mid"))

;; Print metadata
(display "Tracks: ") (display (cdr (assq 'num-tracks data))) (newline)
(display "PPQN: ") (display (cdr (assq 'ppqn data))) (newline)
(display "Duration: ") (display (cdr (assq 'duration data))) (display "ms") (newline)

;; Print events
(define events (cdr (assq 'events data)))
(for-each
  (lambda (event)
    (display event) (newline))
  (take events 10))  ; First 10 events
```

---

## REPL Session Examples

Start the REPL:

```bash
./build/guile_midi
```

### Using Convenience Functions (Recommended)

The simplest way to use guile-midi interactively:

```scheme
> (open)
#<midi-out virtual "guileMIDI">
> (n c4)
> (n e4)
> (n g4)
> (ch (major c4))
> (arp (min7 a3) mf sixteenth)
> (close)
```

### Quick Chord Exploration

```scheme
> (open)
> (ch (major c4))
> (ch (minor c4))
> (ch (dim c4))
> (ch (aug c4))
> (ch (dom7 c4))
> (ch (maj7 c4))
> (ch (min7 c4))
> (close)
```

### Testing Scales

```scheme
> (open)
> (scale c4 'major)
(60 62 64 65 67 69 71)
> (for-each (lambda (p) (n p)) (scale c4 'major))
> (for-each (lambda (p) (n p)) (scale c4 'blues))
> (for-each (lambda (p) (n p)) (scale d4 'dorian))
> (close)
```

### Checking Values

```scheme
> c4
60
> (major c4)
(60 64 67)
> mf
80
> quarter
500
> (bpm 60)
1000
```
