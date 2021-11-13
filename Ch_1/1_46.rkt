#lang sicp



; takes in a method for telling whather a guess is good enough
; and a method for improving a guess
; returns a procedure that takes a guess as argument
; and keeps improving the guess until it is good enough
(define (iterative-improve good-enough improve-guess)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve-guess guess)))
        (if (good-enough guess next) ; should be checking against original value here for sqrt, not new and old guess
            next
            (try next))))
    (try first-guess)))

(define (fixed-point f first-guess)
  ((iterative-improve good-enough f) first-guess))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess next) (good-enough-square guess x))
    (lambda (g) (improve-square g x)))
   1.0))

(define (improve-square guess x)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))
(define (square x)
  (* x x))

(define tolerance 0.00001)
(define (good-enough v1 v2)
  (< (abs (- v1 v2))
     tolerance))

(define tolerance-square 0.001)
(define (good-enough-square guess x)
  (< (abs (- (square guess) x)) tolerance-square))

(cos 0)
(fixed-point cos 1.0)

(sqrt 16)
(sqrt 100)
(sqrt 2378)