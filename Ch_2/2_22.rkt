#lang sicp

(define (square x) (* x x))

; 2.22
; Louis Reasoner tries to rewrite the first square-list procedure of
; Exercise 2.21 so that it evolves an iterative process:
(define (square-list-itr items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
; Unfortunately, defining square-list this way produces the answer
; list in the reverse order of the one desired. Why?

(square-list-itr (list 1 2 3 4))
#|
(iter (1 2) nil)
    (iter (2) 1)
        (iter nil (4 1))
The construction adds the new square to the beginning of the answer list,
as opposed to the end.
|#

; Louis then tries to fix his bug by interchanging the arguments to cons:
(define (square-list-rev items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
; This doesn’t work either. Explain.

(square-list-rev (list 1 2 3 4))
#|
The numbers appear in the right order, however the procedure is appending a new
value at the end of the answer list. This makes a new list, that has two
elements: the previous list, and the current number.
|#