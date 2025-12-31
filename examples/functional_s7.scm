; Functional composition demonstrating s7-midi's thunk-based voices
; Run: ./build/s7_midi examples/functional_s7.scm

(set-tempo! 480)  ; Fast tempo for quick demo
(open)

; Create a voice that plays through a note list
(define (make-melody-player notes vel dur)
  (let ((remaining notes))
    (lambda ()
      (if (null? remaining)
          #f
          (begin
            (n (car remaining) vel dur)
            (set! remaining (cdr remaining))
            dur)))))

; Spawn voices
(spawn (make-melody-player (list c4 e4 g4 c5 g4 e4 c4) mf sixteenth) "melody")
(spawn (make-melody-player (list c2 g2) f eighth) "bass")

(run)

(close)
(display "Functional composition complete!\n")
