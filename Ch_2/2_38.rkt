#lang sicp

; accumulate
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(display "3/2")(newline)
(fold-right / 1 (list 1 2 3))
(display "1/6")(newline)
(fold-left / 1 (list 1 2 3))
(display "(1 (2 (3 ())))")(newline)
(fold-right list nil (list 1 2 3))
(display "(((() 1) 2) 3)")(newline)
(fold-left list nil (list 1 2 3))

(newline)

; order shouldn't matter (associative/commutative)
; a + b = b + a; a * b = b * a

; fold-right: (op x1 init)
; fold-left:  (op init x1)
; must be equal

(display "6")(newline)
(fold-right * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))

(newline)
(fold-right cons nil (list 1 2 3))
(fold-left cons nil (list 1 2 3))