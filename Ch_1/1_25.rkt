#lang sicp

(define (square n) (* n n))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod-new base exp m)
  (remainder (fast-expt base exp) m))

; vs

(define (expmod-old base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod-old base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod-old base (- exp 1) m))
          m))))

(expmod-old 5 5 2)
(expmod-new 5 5 2)

(expmod-old 17 32 13)
(expmod-new 17 32 13)

; Using this new method gets much slower with larger numbers.
; This is because calculating the exponent gets much more difficult
; as the numbers get larger.