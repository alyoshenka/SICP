#lang racket

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


(define (pow b n)
  (display "base: ") (display b) (display " exp: ") (display n) (display "\n")
  (pow-iter b n 1 0))

; iterative exponentiation process using successive squaring
; and requiring a logarithmic number of steps
; b base
; c counter
; p product

; this is iterative, but not logarithmic in number of steps
; introduce the 'shortcut' bit


(define (pow-iter b n a s)
  ;(display "\nbase: ") (display b) (display " n: ") (display n) (display " a: ") (display a) (display "\n")
  (cond
    ((not (> n 0)) (display " steps: ") (display s) (display "\n") a)
    ((= (remainder n 2) 0) (pow-iter (* b b) (/ n 2) a (+ s 1))) ; counter is divisible by 2
    (else (pow-iter b (- n 1) (* a b) (+ s 1))))) ; else normal iter


; tests
(display "\n")
(expt 9 10)

(display "\n")
(pow 9 10)

(display "\n")
(expt 3 15)

(display "\n")
(pow 3 15)

(display "\n")
(pow 3 30)

(display "\n")
(pow 3 60)
