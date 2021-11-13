#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; arithmetic operations
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; constructor and selectors
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


; Exercise 2.1:
; Define a btter version of make-rat that handles both positive and negative arguments.
; make-rat should normalize the sign so that if the rational number is positive,
; both the numerator and denominator are positive, and if
; the rational number is negative, only the numerator is negative.

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

; num and dem pos, num pos dem pos -> nothing
; num pos, dem neg -> num neg, dem pos
; num neg, dem neg -> num pos, dem pos
(define (make-rat-negs n d)
  (let ((g (gcd n d))
        (f (if (< 0 (* n d)) 1 (- 1))))
    (cons (* f (/ n g)) (* f (/ d g)))))

(newline)

(print-rat (make-rat-negs 3 12))
(print-rat (make-rat-negs (- 3) 12))
(print-rat (make-rat-negs 3 (- 12)))
(print-rat (make-rat-negs (- 3) (- 12)))