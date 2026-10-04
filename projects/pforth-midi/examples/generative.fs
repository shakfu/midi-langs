\ generative.fs - defining words, deferred patterns and a random walk
\ Run: ./build/pforth_midi projects/pforth-midi/examples/generative.fs

midi-open
140 set-tempo

\ A defining word: each instrument remembers its channel and program
: instrument ( ch prog "name" -- )
    create , ,
    does> ( -- ) dup cell+ @ to chan  chan swap @ program ;
1 0  instrument piano
2 33 instrument bass

\ A random walk that stays in C minor pentatonic
c4 value walker
: step ( -- )  -3 4 random-range walker +  c4 scale-pentatonic-minor quantize  to walker ;
: walk ( n -- )  0 ?do  step walker note  loop ;

\ PATTERN is deferred: rebind it with IS while the piece is built
defer pattern
: verse  ( -- )  piano eighth to dur  8 walk ;
: bridge ( -- )  bass quarter to dur  c2 note g2 note bb2 note g2 note ;

42 seed
' verse  is pattern  pattern
' bridge is pattern  pattern
' verse  is pattern  pattern

\ RECURSE: a phrase that descends by fourths until it leaves the range
: fall ( pitch -- )  dup c3 > if dup note 5 - recurse else drop then ;
piano sixteenth to dur  c5 fall

midi-close
