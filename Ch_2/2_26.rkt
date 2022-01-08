#lang sicp


; Exercise 2.26: Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

; What result is printed by the interpreter in response to
; evaluating each of the following expressions:

(display "1\n")
(append x y)
(display "(1 2 3 (4 5 6))")(newline)
; append shoves both lists together

(display "\n2\n")
(cons x y)
(display "((1 2 3) (4 5 6))")(newline)
; cons sees the second argument as multiple units

(display "\n3\n")
(list x y)
(display "((1 2 3) (4 5 6))")(newline)
; got this one right