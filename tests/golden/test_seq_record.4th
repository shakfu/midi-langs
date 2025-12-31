\ test_seq_record.4th - Sequence recording mode (seq-start/seq-end)
\ Each test prints -1 for pass, 0 for fail

\ Open virtual MIDI port
midi-open

\ Set tempo and duration
120 bpm!
500 dur!

\ Test basic seq-start/seq-end
0 seq-start
seq@ 0 = .

\ Record some notes
c4, e4, g4,
0 seq-end

\ Check we recorded 6 events (3 notes * 2 events each)
seq-length 6 = .

\ Clear and record again
seq-clear
1 seq-start
seq@ 1 = .

\ Record a chord
(c4 e4 g4),
1 seq-end

\ Check we recorded 6 events (3 note-ons + 3 note-offs)
1 seq
seq-length 6 = .

\ Test dynamics apply during recording
2 seq-start
ff
c4,
2 seq-end

\ Verify sequence has 2 events
2 seq
seq-length 2 = .

\ Test duration changes work during recording
3 seq-start
200 dur!
c4, e4,
3 seq-end

3 seq
seq-length 4 = .

\ Clean up
midi-close

\ Print marker
999 .
