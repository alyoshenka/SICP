#lang sicp

; Exercise 2.23:  Give an implementation of for-each.

(define (for-each func args)
  (func (car args))
  (if (not(null? (cdr args)))
      (for-each func (cdr args))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

(newline)(newline)
(for-each (lambda (x)
            (display (* x x))
            (newline))
          (list 1 2 3 4))