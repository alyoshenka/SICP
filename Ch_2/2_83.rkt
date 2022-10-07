#lang racket

#|
Exercise 2.83: Suppose you are designing a generic arith-
metic system for dealing with the tower of types shown in
Figure 2.25: integer, rational, real, complex. For each type
(except complex), design a procedure that raises objects of
that type one level in the tower. Show how to install a
generic raise operation that will work for each type (ex-
cept complex).
|#

#|
"raise" = 'coerce
|#

(require "./generic_arithmetic_package.rkt")

(define (install-raise-package)
  (define (put-coercion from to func)
    (put 'coercion (list from to) func))
  (define (get-coercion from to)
    (get 'coercion (list from to)))
  'done)