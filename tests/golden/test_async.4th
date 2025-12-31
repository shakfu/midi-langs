\ test_async.4th - Async sequence playback
\ Each test prints -1 for pass, 0 for fail

\ Open virtual MIDI port
midi-open

\ Set up tempo and duration
120 bpm!
250 dur!

\ Test seq-start creates sequence and doesn't pollute stack
depth 0 = .
0 seq-start
depth 0 = .
80 vel!
c4, e4,
0 seq-end

\ Verify current sequence is 0
seq@ 0 = .

\ Create second sequence
1 seq-start
seq@ 1 = .
70 vel!
125 dur!
g4, a4,
1 seq-end

\ Start first sequence playing async
0 seq seq-play&

\ Check that something is playing
seq-playing? .

\ Check active count is 1
seq-active 1 = .

\ Start second sequence - now both playing
1 seq seq-play&

\ Check active count is 2
seq-active 2 = .

\ Stop just sequence 0
0 seq seq-stop

\ Wait a tick
10 ms

\ Stop all
seq-stop-all

\ After stop, nothing playing
10 ms
seq-playing? not .
seq-active 0 = .

\ Test seq-loop& starts looping playback
0 seq seq-loop&
seq-playing? .
seq-stop-all

\ Clean up
midi-close

\ Print marker
999 .
