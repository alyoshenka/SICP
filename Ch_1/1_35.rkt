#lang sicp

; φ^2 = φ + 1
; φ = 1 + 1/φ


(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "1.618033...")(newline)
(fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0)