0#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess steps)
    (display "steps: ")(display steps)(newline)
    (let ((next (f guess)))
      (display "guess: ")(display guess)(display " next: ")(display next)(newline)
      (if (close-enough? guess next)
          next
          (try next (+ steps 1)))))
  (try first-guess 1))


(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(newline)
(fixed-point (lambda (x) (+ (/ (log 1000) (* 2 (log x))) (/ x 2))) 2.0)

; 34 steps, 9 steps