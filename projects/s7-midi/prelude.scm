;; Dynamics (velocity values)
(define ppp 16)
(define pp 33)
(define p 49)
(define mp 64)
(define mf 80)
(define f 96)
(define ff 112)
(define fff 127)

;; Durations (milliseconds at 120 BPM)
(define whole 2000)
(define half 1000)
(define quarter 500)
(define eighth 250)
(define sixteenth 125)

(define (dotted dur) (floor (* dur 1.5)))

;; Tempo
(define *bpm* 120)

(define (set-tempo! bpm)
  (set! *bpm* bpm)
  (let ((beat-ms (floor (/ 60000 bpm))))
    (set! quarter beat-ms)
    (set! half (* beat-ms 2))
    (set! whole (* beat-ms 4))
    (set! eighth (floor (/ beat-ms 2)))
    (set! sixteenth (floor (/ beat-ms 4)))))

(define (get-tempo) *bpm*)

(define (bpm tempo) (floor (/ 60000 tempo)))

;; Pitch constants
(define c0 12) (define cs0 13) (define db0 13) (define d0 14)
(define ds0 15) (define eb0 15) (define e0 16) (define f0 17)
(define fs0 18) (define gb0 18) (define g0 19) (define gs0 20)
(define ab0 20) (define a0 21) (define as0 22) (define bb0 22) (define b0 23)

(define c1 24) (define cs1 25) (define db1 25) (define d1 26)
(define ds1 27) (define eb1 27) (define e1 28) (define f1 29)
(define fs1 30) (define gb1 30) (define g1 31) (define gs1 32)
(define ab1 32) (define a1 33) (define as1 34) (define bb1 34) (define b1 35)

(define c2 36) (define cs2 37) (define db2 37) (define d2 38)
(define ds2 39) (define eb2 39) (define e2 40) (define f2 41)
(define fs2 42) (define gb2 42) (define g2 43) (define gs2 44)
(define ab2 44) (define a2 45) (define as2 46) (define bb2 46) (define b2 47)

(define c3 48) (define cs3 49) (define db3 49) (define d3 50)
(define ds3 51) (define eb3 51) (define e3 52) (define f3 53)
(define fs3 54) (define gb3 54) (define g3 55) (define gs3 56)
(define ab3 56) (define a3 57) (define as3 58) (define bb3 58) (define b3 59)

(define c4 60) (define cs4 61) (define db4 61) (define d4 62)
(define ds4 63) (define eb4 63) (define e4 64) (define f4 65)
(define fs4 66) (define gb4 66) (define g4 67) (define gs4 68)
(define ab4 68) (define a4 69) (define as4 70) (define bb4 70) (define b4 71)

(define c5 72) (define cs5 73) (define db5 73) (define d5 74)
(define ds5 75) (define eb5 75) (define e5 76) (define f5 77)
(define fs5 78) (define gb5 78) (define g5 79) (define gs5 80)
(define ab5 80) (define a5 81) (define as5 82) (define bb5 82) (define b5 83)

(define c6 84) (define cs6 85) (define db6 85) (define d6 86)
(define ds6 87) (define eb6 87) (define e6 88) (define f6 89)
(define fs6 90) (define gb6 90) (define g6 91) (define gs6 92)
(define ab6 92) (define a6 93) (define as6 94) (define bb6 94) (define b6 95)

(define c7 96) (define cs7 97) (define db7 97) (define d7 98)
(define ds7 99) (define eb7 99) (define e7 100) (define f7 101)
(define fs7 102) (define gb7 102) (define g7 103) (define gs7 104)
(define ab7 104) (define a7 105) (define as7 106) (define bb7 106) (define b7 107)

(define c8 108) (define cs8 109) (define db8 109) (define d8 110)
(define ds8 111) (define eb8 111) (define e8 112) (define f8 113)
(define fs8 114) (define gb8 114) (define g8 115) (define gs8 116)
(define ab8 116) (define a8 117) (define as8 118) (define bb8 118) (define b8 119)

;; Chord builders
(define (major root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 7))))

(define (minor root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 3) (+ r 7))))

(define (dim root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 3) (+ r 6))))

(define (aug root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 8))))

(define (dom7 root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 7) (+ r 10))))

(define (maj7 root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 4) (+ r 7) (+ r 11))))

(define (min7 root)
  (let ((r (if (number? root) root (note root))))
    (list r (+ r 3) (+ r 7) (+ r 10))))

;; Transpose helpers
(define (transpose pitch semitones)
  (let ((p (if (number? pitch) pitch (note pitch))))
    (+ p semitones)))

(define (octave-up pitch) (transpose pitch 12))
(define (octave-down pitch) (transpose pitch -12))

;; Arpeggio helper
(define (midi-arpeggio m pitches . args)
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) eighth))
        (ch (if (>= (length args) 3) (caddr args) 1)))
    (for-each (lambda (p) (midi-note m p vel dur ch)) pitches)))

;; Rest helper
(define (rest . args)
  (let ((dur (if (null? args) quarter (car args))))
    (midi-sleep dur)))

;; ============================================================================
;; REPL convenience functions (use global *midi* output)
;; ============================================================================
(define *midi* #f)

(define (open . args)
  "Open MIDI port and set as default. (open) or (open name) or (open index)"
  (if *midi* (midi-close *midi*))
  (set! *midi* (if (null? args) (midi-open) (midi-open (car args))))
  *midi*)

(define (close)
  "Close default MIDI port"
  (if *midi* (begin (midi-close *midi*) (set! *midi* #f))))

(define (n pitch . args)
  "Play note on default port: (n pitch [vel] [dur] [ch])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) quarter))
        (ch (if (>= (length args) 3) (caddr args) 1)))
    (midi-note *midi* pitch vel dur ch)))

(define (ch pitches . args)
  "Play chord on default port: (ch pitches [vel] [dur] [channel])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) quarter))
        (channel (if (>= (length args) 3) (caddr args) 1)))
    (midi-chord *midi* pitches vel dur channel)))

(define (arp pitches . args)
  "Arpeggiate on default port: (arp pitches [vel] [dur] [ch])"
  (if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
  (let ((vel (if (>= (length args) 1) (car args) mf))
        (dur (if (>= (length args) 2) (cadr args) eighth))
        (channel (if (>= (length args) 3) (caddr args) 1)))
    (midi-arpeggio *midi* pitches vel dur channel)))

(define (play-notes ns) 
	"Play a list of notes"
	(if (not *midi*) (error 'no-midi-port "No MIDI port open. Use (open) first."))
	(map n ns))

