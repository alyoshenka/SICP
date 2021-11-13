#lang racket
; x -> number to take cubed root of (orig)
; y -> current approximation (guess)
; better approximation -> ((x/y^2) + 2y)/3

; return a better guess
(define (improve-guess orig guess)
  (/ (+ (* 2 guess) (/ orig (* guess guess))) 3))

; cubed root, start function
(define (cube-rt orig) (cube-rt-iter orig 1.0))

; cubed root iteration
(define (cube-rt-iter orig guess)
  ;(display "guess: ")
  ;(display guess)
  ;(display "\n")
  (if (good-enough? orig guess)
      ; if clause
      guess
      ; then clause
      (cube-rt-iter orig (improve-guess orig guess))))

(define eps 0.001)
  
; close enough
(define (good-enough? orig guess)
  (> eps (diff (* guess guess guess) orig)))

(define (diff a b)
  (abs (- a b)))

; absolute value
(define (abs x)
  (if (< x 0)
      (- x)
      x
   )
)

1
(cube-rt 1)
2
(cube-rt (* 2 2 2))
3
(cube-rt 27)
45
(cube-rt (* 45 45 45))
0.00078
(cube-rt (* 0.00078 0.00078 0.00078))
