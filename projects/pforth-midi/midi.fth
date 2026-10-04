\ midi.fth - MIDI vocabulary for pforth-midi, compiled into the static dictionary.
\ C words (midi_words.c) named (X) return an ior; the words here check it.
\ Channels are 1-16. Pitches are MIDI note numbers; C4 = 60.

decimal

\ ---------------------------------------------------------------- errors

: ?midi ( ior -- )  abort" MIDI: no port open, or value out of range" ;

\ ---------------------------------------------------------------- pitch literals

\ NUMBER? tries an integer in BASE, then a pitch name (c4 C#4 cs4 Db4 c-1 g9),
\ then a float. Pitches before floats: >FLOAT accepts e4 as 0e4.
\ Integers first: in HEX, c4 is still $C4.
: midi.number? ( $addr -- 0 | n 1 | d 2 | r 3 )
    dup >r (number?) ?dup if r> drop exit then
    r@ count (parse-pitch) dup 0< 0= if r> drop 1 exit then drop
    r> (fp.number?)
;
' midi.number? is number?

: parse-pitch ( c-addr u -- pitch | -1 )  (parse-pitch) ;

: transpose   ( pitch n -- pitch' )  + ;
: octave-up   ( pitch -- pitch' )  12 + ;
: octave-down ( pitch -- pitch' )  12 - ;

\ ---------------------------------------------------------------- ports

: midi-open-port ( n -- )  (midi-open-port) abort" midi-open-port: cannot open port" ;
: midi-open-as ( c-addr u -- )  (midi-open-virtual) abort" midi-open-as: cannot create port" ;
: midi-open ( -- )  s" pForthMIDI" midi-open-as ;

\ ---------------------------------------------------------------- defaults

80  value vel     \ velocity for NOTE, CHORD, ARPEGGIO
500 value dur     \ duration in ms
1   value chan    \ channel 1-16

16  constant ppp   33 constant pp   49 constant p    64 constant mp
80  constant mf    96 constant f   112 constant ff  127 constant fff

\ ---------------------------------------------------------------- messages

: note-on    ( pitch vel ch -- )  (note-on) ?midi ;
: note-off   ( pitch ch -- )  (note-off) ?midi ;
: cc         ( ch ctl val -- )  (cc) ?midi ;
: program    ( ch prog -- )  (program) ?midi ;
: pitch-bend ( ch val -- )  (pitch-bend) ?midi ;   \ 0-16383, 8192 = centre
: pitch-bend-cents ( ch cents -- )  cents>bend pitch-bend ;
: all-notes-off ( ch -- )  (all-notes-off) ?midi ;

\ ---------------------------------------------------------------- playing

: note ( pitch -- )  dup vel chan note-on  dur ms  chan note-off ;

\ A chord or scale on the stack is its pitches followed by their count.
: chord ( p1 .. pn n -- )
    dup 0 ?do  dup i - pick vel chan note-on  loop
    dur ms
    0 ?do  chan note-off  loop
;
: arpeggio ( p1 .. pn n -- )
    dup 0 ?do  dup i - pick note  loop
    0 ?do  drop  loop
;

: n   ( pitch -- )  note ;
: ch  ( p1 .. pn n -- )  chord ;
: arp ( p1 .. pn n -- )  arpeggio ;

\ ---------------------------------------------------------------- timing

120 value tempo   \ BPM

: set-tempo ( bpm -- )  20 max 300 min to tempo ;
: bpm       ( tempo -- ms )  60000 swap / ;   \ quarter-note length
: quarter   ( -- ms )  tempo bpm ;
: whole     ( -- ms )  quarter 4 * ;
: half      ( -- ms )  quarter 2* ;
: eighth    ( -- ms )  quarter 2/ ;
: sixteenth ( -- ms )  quarter 4 / ;
: dotted    ( ms -- ms' )  3 * 2/ ;
: rest      ( ms -- )  ms ;

\ ---------------------------------------------------------------- intervals

\ An interval table is a count cell followed by that many cells.
\ SCALE: and CHORD: start one; END-TABLE stores its count.
: end-table ( a -- )  here over - 1 cells - 1 cells /  swap ! ;
: table@    ( a -- addr n )  dup cell+ swap @ ;

: build-scale ( root addr n -- p1 .. pn n )
    dup >r 0 ?do  2dup i cells + @ +  -rot  loop  2drop r>
;

\ scale: name i1 , i2 , ... end-table     name ( -- addr n )
: scale: ( "name" -- a )  create here 0 ,  does> ( -- addr n ) table@ ;
\ chord: name i1 , i2 , ... end-table     name ( root -- p1 .. pn n )
: chord: ( "name" -- a )  create here 0 ,  does> ( root -- p1 .. pn n ) table@ build-scale ;

chord: major      0 , 4 , 7 ,       end-table
chord: minor      0 , 3 , 7 ,       end-table
chord: dim        0 , 3 , 6 ,       end-table
chord: aug        0 , 4 , 8 ,       end-table
chord: dom7       0 , 4 , 7 , 10 ,  end-table
chord: maj7       0 , 4 , 7 , 11 ,  end-table
chord: min7       0 , 3 , 7 , 10 ,  end-table
chord: dim7       0 , 3 , 6 , 9 ,   end-table
chord: half-dim7  0 , 3 , 6 , 10 ,  end-table
chord: sus2       0 , 2 , 7 ,       end-table
chord: sus4       0 , 5 , 7 ,       end-table

scale: scale-major             0 , 2 , 4 , 5 , 7 , 9 , 11 ,  end-table
scale: scale-minor             0 , 2 , 3 , 5 , 7 , 8 , 10 ,  end-table
scale: scale-dorian            0 , 2 , 3 , 5 , 7 , 9 , 10 ,  end-table
scale: scale-phrygian          0 , 1 , 3 , 5 , 7 , 8 , 10 ,  end-table
scale: scale-lydian            0 , 2 , 4 , 6 , 7 , 9 , 11 ,  end-table
scale: scale-mixolydian        0 , 2 , 4 , 5 , 7 , 9 , 10 ,  end-table
scale: scale-locrian           0 , 1 , 3 , 5 , 6 , 8 , 10 ,  end-table
scale: scale-harmonic-minor    0 , 2 , 3 , 5 , 7 , 8 , 11 ,  end-table
scale: scale-melodic-minor     0 , 2 , 3 , 5 , 7 , 9 , 11 ,  end-table
scale: scale-pentatonic        0 , 2 , 4 , 7 , 9 ,           end-table
scale: scale-pentatonic-minor  0 , 3 , 5 , 7 , 10 ,          end-table
scale: scale-blues             0 , 3 , 5 , 6 , 7 , 10 ,      end-table
scale: scale-whole-tone        0 , 2 , 4 , 6 , 8 , 10 ,      end-table
scale: scale-chromatic  0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 ,  end-table

\ C words: SCALE-DEGREE ( root addr n deg -- pitch )  degrees are 1-based
\          IN-SCALE?    ( pitch root addr n -- flag )
\          QUANTIZE     ( pitch root addr n -- pitch' )

\ ---------------------------------------------------------------- files

: write-mid ( c-addr u -- )  (write-mid) abort" write-mid: failed" ;
: read-mid  ( c-addr u -- )  (read-mid) abort" read-mid: failed" ;

\ ---------------------------------------------------------------- startup

\ pForth's history ACCEPT echoes every key; with piped input that
\ repeats the script into the output.
: auto.init ( -- )  auto.init  stdin-tty? 0= if history.off then ;

\ ---------------------------------------------------------------- help

: midi-help ( -- )
    cr ." Ports:    midi-list  midi-open  s" [char] " emit ."  name" [char] " emit ."  midi-open-as  n midi-open-port"
    cr ."           midi-close  midi-open?  panic"
    cr ." Play:     pitch note   p1..pn n chord   p1..pn n arpeggio   (n ch arp)"
    cr ."           pitch vel ch note-on   pitch ch note-off"
    cr ."           ch ctl val cc   ch prog program   ch val pitch-bend"
    cr ." Defaults: vel dur chan tempo (VALUEs: 100 to vel)   ppp .. fff"
    cr ." Pitches:  c4 C#4 db4 c-1 .. g9   transpose octave-up octave-down .pitch parse-pitch"
    cr ." Chords:   root major minor dim aug dom7 maj7 min7 dim7 half-dim7 sus2 sus4"
    cr ." Scales:   scale-major .. scale-chromatic ( -- addr n )"
    cr ."           root addr n build-scale   root addr n deg scale-degree"
    cr ."           pitch root addr n in-scale? / quantize"
    cr ."           scale: name 0 , 2 , ... end-table   (chord: likewise)"
    cr ." Timing:   ms rest set-tempo bpm whole half quarter eighth sixteenth dotted"
    cr ." Record:   bpm record-start  record-stop  s" [char] " emit ."  f.mid" [char] " emit ."  write-mid  read-mid"
    cr ." Random:   lo hi random-range  n seed"
    cr
;
