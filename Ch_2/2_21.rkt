#lang sicp

(define (square x) (* x x))

; 2.21
; Here are two different definitions of square-list.
; Complete both of them by filling in the missing expressions:

(define (square-list1 items)
  (if (null? items)
      nil
      (cons
       (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map
   (lambda (x) (* x x)) items))

(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))