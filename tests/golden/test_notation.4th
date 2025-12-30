\ test_notation.4th - Pitch parsing and notation features
\ Each test prints -1 for pass, 0 for fail

\ Pitch parsing tests
c4 60 = .
C4 60 = .
a4 69 = .
c5 72 = .
c0 12 = .
c#4 61 = .
db4 61 = .
f#3 54 = .
bb3 58 = .

\ All note names at octave 4
d4 62 = .
e4 64 = .
f4 65 = .
g4 67 = .
b4 71 = .

\ Sharp notes
d#4 63 = .
f#4 66 = .
g#4 68 = .
a#4 70 = .

\ Flat notes
eb4 63 = .
gb4 66 = .
ab4 68 = .

\ Octave commands (^ = up, v = down, work on current_pitch)
\ Default current_pitch is 60
^ 72 = .
v 48 = .

\ Set defaults (these just set internal state, verify they don't crash)
5 ch!
100 vel!
250 dur!
75 gate!

\ gate@ should return the set value
gate@ 75 = .

\ BPM setting
90 bpm!
bpm@ 90 = .

\ Reset defaults
1 ch! 80 vel! 500 dur! 100 gate! 120 bpm!

\ Print marker
999 .
