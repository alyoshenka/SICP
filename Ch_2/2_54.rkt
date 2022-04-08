#lang sicp

; a and b are equal? if they are both symbols
; and the symbols are eq?, or if they are both lists
; such that (car a) is equal? to (car b) and
; (cdr a) is equal? to (cdr b).

(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((eq? a b) #t)
        (else #f)))

(display "#t ")(equal? '(this is a list) '(this is a list))
(display "#f ")(equal? '(this is a list) '(this (is a) list))