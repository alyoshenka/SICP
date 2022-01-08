#lang sicp

#| 
Exercise 2.28: Write a procedure fringe that takes as argu-
ment a tree (represented as a list) and returns a list whose
elements are all the leaves of the tree arranged in left-to-
right order. For example, |#

(define (fringe tree)

  (define (branch tree items)
    (cond
      ((pair? tree) (branch (cdr tree) (branch (car tree) items)))
      ((null? tree) items)
      (else (append items (list tree)))))
  
  (branch tree nil))
      

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(display "(1 2 3 4)")(newline)(newline)
(fringe (list x x))
(display "(1 2 3 4 1 2 3 4)")(newline)(newline)
