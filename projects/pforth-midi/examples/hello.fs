\ hello.fs - scale, chords and a progression
\ Run: ./build/pforth_midi projects/pforth-midi/examples/hello.fs

midi-open                         \ virtual port "pForthMIDI"
200 to dur

c4 scale-major build-scale arpeggio

: progression ( -- )
    c4 major chord  a3 minor chord  f3 major chord  g3 dom7 chord ;

quarter to dur
progression
c4 major chord

midi-close
