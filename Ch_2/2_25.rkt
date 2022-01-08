#lang sicp
; Exercise 2.25: Give combinations of cars and cdrs that
; will pick 7 from each of the following lists:

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(display "(1 3 (5 7) 9)")(newline)
l1
(display "((7))")(newline)
l2
(display "(1 (2 (3 (4 (5 (6 7))))))")(newline)
l3

; 1
(car (cdr (car (cddr l1))))
(car(cdaddr l1))
; 2
(car (car l2))
(caar l2)
; 3
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))