\ test_sequences.4th - Sequence operations
\ Each test prints -1 for pass, 0 for fail

\ Create a new sequence
seq-new 0 = .

\ Select it
0 seq seq@ 0 = .

\ Add notes to sequence: time pitch vel dur seq-note
0 c4 80 240 seq-note
240 e4 80 240 seq-note
480 g4 80 480 seq-note

\ Check length (3 note-on + 3 note-off = 6 events)
seq-length 6 = .

\ Create second sequence
seq-new 1 = .

1 seq seq@ 1 = .

\ Add to second sequence
0 a4 80 120 seq-note
seq-length 2 = .

\ Go back to first
0 seq

\ Test BPM
120 bpm! bpm@ 120 = .
90 bpm! bpm@ 90 = .

\ Reset
120 bpm!

\ Print marker
999 .
