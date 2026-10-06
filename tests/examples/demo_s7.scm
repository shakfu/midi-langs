;; README.md example for s7-midi
;; Tests the quick example from the project README

(open)

;; Functional transformations
(define melody (scale c4 'dorian))
(define bass (invert melody c4))  ; melodic inversion

;; Thunk-based concurrent voices
(spawn (make-melody-voice melody mf eighth) "melody")
(spawn (make-melody-voice bass ff quarter) "bass")

;; Euclidean rhythm: 3 hits over 8 steps, one (action . delay) per step
(define (drum-step hit)
  (cons (lambda ()
          (midi-note-off *midi* c2 1)
          (if hit (midi-note-on *midi* c2 ff 1)))
        sixteenth))
(spawn (make-sequence-voice (map drum-step (euclidean 3 8))) "rhythm")

(run)
(close)
