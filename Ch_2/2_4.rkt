#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 1 2))
(define x (cons 1 2))
(car x)

; cons takes in two inputs, x and y, then constructs and returns
; a lambda procedure that calls itself with x and y as inputs.
; car takes in a procedure and calls that procedure with a lambda
; procedure with 2 arguments, returning the first of the 2.
; When calling car with the procedure returned by cons (z) as an argument,
; z calls the lambda function within car, passing the original x any y values
; as inputs. car then returns the first input, x.