#lang sicp

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (g) (lambda (y) (g ((n g) y)))))

; define one and two directly
; (use substitution to evaluate (add-1 zero))

; (lambda (<formal-parameters>) <body>)
; the procedure of an argument <formal-parameters> that does <body>

; (add-1 0)


((lambda (x) x) 0) ; -> 0
((lambda (f) (lambda (x) x) 1) 2) ; -> 1
((lambda (f) (lambda (x) x) 5) 9) ; -> 5

zero
(add-1 0)
(add-1 zero)


(define (one (lambda (f) (lambda (x) (f x)))))
(define (two (lambda (f) (lambda (x) (f (f x))))))