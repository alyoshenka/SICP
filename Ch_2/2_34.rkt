#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

#|
Exercise 2.34:
Fill in the following template to produce a procedure that
evaluates a polynomial using Horner’s rule. Assume that
the coefficients of the polynomial are arranged in a sequence,
from a0 through an.
|#

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

; test
(display "79")
(newline)
(horner-eval 2 (list 1 3 0 5 0 1))