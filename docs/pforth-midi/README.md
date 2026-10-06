# pforth-midi

[pForth](https://github.com/philburk/pforth), a portable ANS-style Forth, with MIDI words. All of standard Forth is available: `create`/`does>`, `immediate`, `'`/`execute`, `defer`/`is`, the return stack, locals, exceptions and floats. Words compile to threaded code, so names bind at definition time.

stack-midi is a separate, custom Forth-like language. The two do not share syntax; see [Differences from stack-midi](#differences-from-stack-midi).

## Build and run

Linux and macOS only; CMake skips pforth-midi on Windows until it has been built there. Only Linux has been tested.

```sh
make pforth-midi
./build/pforth_midi                 # interactive prompt; BYE exits
./build/pforth_midi song.fs         # run a file, then exit
./build/pforth_midi -q              # no banner
```

At the prompt, `midi-help` lists the MIDI words and `words` lists all words.

```forth
midi-list                 \ ports, as "client: port"
midi-open                 \ virtual port "pForthMIDI"; or: 1 midi-open-port
c4 note                   \ play C4 for DUR ms at VEL on CHAN
c4 major chord            \ C E G together
c4 scale-major build-scale arpeggio
100 to vel  250 to dur  2 to chan
```

## Vocabulary

Stack effects use Forth notation. Channels are 1-16. Pitches are MIDI note numbers; C4 = 60. A chord or scale on the stack is its pitches followed by their count: `c4 major` leaves `60 64 67 3`.

### Ports

| Word | Stack | Description |
|-|-|-|
| `midi-list` | `( -- )` | Print output ports as `index: client: port` |
| `midi-port-count` | `( -- n )` | Number of output ports |
| `midi-open` | `( -- )` | Create virtual port `pForthMIDI` |
| `midi-open-as` | `( c-addr u -- )` | Create a virtual port: `s" Name" midi-open-as` |
| `midi-open-port` | `( n -- )` | Open port N from `midi-list` |
| `midi-close` | `( -- )` | All notes off on 16 channels, then close |
| `midi-open?` | `( -- flag )` | True if a port is open |
| `panic` | `( -- )` | All notes off on 16 channels |

### Messages and playing

| Word | Stack | Description |
|-|-|-|
| `note-on` | `( pitch vel ch -- )` | Note On |
| `note-off` | `( pitch ch -- )` | Note Off |
| `cc` | `( ch ctl val -- )` | Control Change |
| `program` | `( ch prog -- )` | Program Change |
| `pitch-bend` | `( ch val -- )` | 0-16383, 8192 = centre |
| `pitch-bend-cents` | `( ch cents -- )` | Bend by cents, +/- 2 semitone range |
| `cents>bend` | `( cents -- val )` | Cents to a 0-16383 bend value |
| `all-notes-off` | `( ch -- )` | CC 123 on one channel |
| `note`, `n` | `( pitch -- )` | Note On, wait `dur`, Note Off |
| `chord`, `ch` | `( p1 .. pn n -- )` | All notes together for `dur` |
| `arpeggio`, `arp` | `( p1 .. pn n -- )` | Each note in turn for `dur` |

Words that send a message abort with `MIDI: no port open, or value out of range` when the send fails.

### Defaults and dynamics

`vel` (80), `dur` (500 ms), `chan` (1) and `tempo` (120) are VALUEs: `100 to vel`. `ppp pp p mp mf f ff fff` are 16, 33, 49, 64, 80, 96, 112, 127.

### Pitches

Pitch names are numbers: `c4` `C#4` `cs4` `Db4` `bb3` `c-1` `g9`, case-insensitive, in interpreted and compiled code. Conversion tries an integer in `BASE`, then a pitch name, then a float. So in `HEX`, `c4` is `$C4`, and `1e4` is still a float.

| Word | Stack |
|-|-|
| `transpose` | `( pitch n -- pitch' )` |
| `octave-up`, `octave-down` | `( pitch -- pitch' )` |
| `.pitch` | `( pitch -- )` prints `C#4` |
| `parse-pitch` | `( c-addr u -- pitch \| -1 )`: `s" Db4" parse-pitch` |

### Chords and scales

Chords: `major minor dim aug dom7 maj7 min7 dim7 half-dim7 sus2 sus4`, each `( root -- p1 .. pn n )`.

Scales: `scale-major scale-minor scale-dorian scale-phrygian scale-lydian scale-mixolydian scale-locrian scale-harmonic-minor scale-melodic-minor scale-pentatonic scale-pentatonic-minor scale-blues scale-whole-tone scale-chromatic`, each `( -- addr n )`.

| Word | Stack |
|-|-|
| `build-scale` | `( root addr n -- p1 .. pn n )` |
| `scale-degree` | `( root addr n deg -- pitch )`; 1-based, 9 = second an octave up |
| `in-scale?` | `( pitch root addr n -- flag )` |
| `quantize` | `( pitch root addr n -- pitch' )` |

Define new ones with `scale:` and `chord:`:

```forth
midi-open
scale: scale-hirajoshi  0 , 2 , 3 , 7 , 8 ,  end-table
chord: power            0 , 7 , 12 ,         end-table
a2 power chord
```

### Timing

| Word | Stack | Description |
|-|-|-|
| `set-tempo` | `( bpm -- )` | Clamped to 20-300 |
| `bpm` | `( tempo -- ms )` | Quarter-note length |
| `whole half quarter eighth sixteenth` | `( -- ms )` | At the current tempo |
| `dotted` | `( ms -- ms' )` | 1.5x |
| `ms`, `rest` | `( ms -- )` | Wait |

`dur` does not follow `tempo`; set it with `quarter to dur`.

### Recording, files, random

| Word | Stack | Description |
|-|-|-|
| `record-start` | `( bpm -- )` | Record sent notes and CCs |
| `record-stop` | `( -- n )` | Stop; event count |
| `record-count`, `recording?` | `( -- n )`, `( -- flag )` | |
| `write-mid` | `( c-addr u -- )` | Save the recording as a MIDI file |
| `read-mid` | `( c-addr u -- )` | Print a MIDI file's events |
| `random-range` | `( lo hi -- n )` | Inclusive |
| `seed` | `( n -- )` | Seed `random-range` |

pForth's own `random` is separate and unaffected by `seed`.

## Extending it

Ordinary Forth defines new kinds of words. From `projects/pforth-midi/examples/generative.fs`:

```forth norun
: instrument ( ch prog "name" -- )
    create , ,
    does> ( -- ) dup cell+ @ to chan  chan swap @ program ;
2 33 instrument bass

defer pattern
' verse is pattern  pattern
```

## Differences from stack-midi

| stack-midi | pforth-midi | Reason |
|-|-|-|
| `c4,` plays a note | `c4 note` | `,` compiles a cell |
| `[ c4 e4 ]` sequence | `c4 e4 2 arpeggio` | `[` and `]` switch compile state |
| `{ ... }` block | `:noname ... ;` and `execute` | `{` declares locals |
| `vel=100` | `100 to vel` | Not a number or a word |
| `name 4 times` | `4 0 do name loop` | No implicit "last word" |
| redefinition changes callers | callers keep the old word; use `defer` | Names bind at definition time |

stack-midi features not ported: async sequences, bracket notation, articulation suffixes, probability syntax, input ports, `save-source`.

## Implementation

- `thirdparty/pforth` is pForth at commit `e5617638c3a397c644fe82c6301711867a0ca1bf` (2026-07-28, version 2.2.0), unmodified.

- `projects/pforth-midi/midi_words.c` adds the C words through pForth's `CustomFunctionTable`. MIDI I/O is mhs-midi's `midi_ffi.c`, compiled in.

- `projects/pforth-midi/midi.fth` defines the rest in Forth. The build compiles it into pForth's dictionary and embeds the result as C (`PF_STATIC_DIC`), so the binary needs no `.dic` file.

- `projects/pforth-midi/pf_io_midi.c` replaces two terminal-input functions; its header says why.
