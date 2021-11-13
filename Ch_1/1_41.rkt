#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))

(display 7)(newline)
((double inc) 5)

; the answer is not 13, it is 21
; because...
; double double -> 4
; double (d d) -> d d d d -> 2^4 = 16

(display 21)(newline)
(((double (double double)) inc) 5)

(((double (double double)) inc) 0)