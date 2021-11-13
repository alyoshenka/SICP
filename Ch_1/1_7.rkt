#lang racket

; an iteration of Newton's square root method
; if within bounds, returns guess
; else computes another iteration
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

; improved guess = (previous guess + (given number / previous guess)) / 2
(define (improve guess x)
  (average guess (/ x guess)))

; average of two numbers
(define (average x y)
  (/ (+ x y) 2)
)

; error tolerance
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; absolute value
(define (abs x)
  (if (< x 0)
      (- x)
      x
   )
)

; squared function
(define (square x)
  (* x x))

; gotta start somewhere
(define (sqrt x)
  (sqrt-iter 1.0 x))

; 0.01%
(define frac 0.0001)

; better sqrt
(define (sqrt-better x)
  (sqrt-iter-better 1.0 2.0 x))

; better sqrt iter
(define (sqrt-iter-better guess change x)
  (if (good-enough-better? guess change)
      guess
      (sqrt-iter-better (improve guess x) (- guess (improve guess x)) x)))

(define (good-enough-better? guess change)
  (< (abs(/ change guess)) frac))

(sqrt 9)
(sqrt 25)
(sqrt 0)
(sqrt 1)

0.0001
(sqrt 0.00000001)
20
(sqrt 400)
100000
(sqrt 10000000000)
1234567890
(sqrt 1524157875019052100)

"sqrt better"
1
(sqrt-better 1)
0.00001
(sqrt-better 0.0000000001)
100000
(sqrt-better 10000000000)

