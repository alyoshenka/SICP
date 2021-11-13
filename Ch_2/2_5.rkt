#lang sicp

; Exercise 2.5
; Show that we can represent pairs of nonnegative integers
; using only numbers and arithmetic operations if we represent
; the pair a and b as the integer that is the product (2^a)(3^b)
; Give the corresponding definitions of the procedures cons, car, and cdr.

(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (cons a b)  (* (expt 2 a) (expt 3 b)))
(define (car p) (divs-before-rem? p 2))
(define (cdr p) (divs-before-rem? p 3))

; num: original product
; den: number to check
(define (divs-before-rem? num den)
  (define (div quo sum)
    (if (= 0 (remainder quo den))
           (div (/ quo den) (+ sum 1))
           sum))
  (div num 0))

(define p (cons 5 8))
(car p)
(cdr p)

; divide by the desired base until there is a remainder,
; the number of times divided is the power