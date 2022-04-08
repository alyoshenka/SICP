#lang sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(display "#f ")(memq 'apple '(pear banana prune))
(display "(apple pear) ")(memq 'apple '(x (apple sauce) y apple pear))

; 2.53
(newline)

; What would the interpreter print in response
; to evaluating each of the following expressions?
(display "(a b c)")(newline)
(list 'a 'b 'c)
(display "((george))")(newline)
(list (list 'george))
(display "(y1 y2)")(newline) ; ((y1 y2))
(cdr '((x1 x2) (y1 y2)))
(display "y1")(newline) ; (y1 y2)
(cadr '((x1 x2) (y1 y2)))
(display "#t")(newline) ; #f
(pair? (car '(a short list)))
(display "#f")(newline)
(memq 'red '((red shoes) (blue socks)))
(display "(red shoes blue socks)")(newline)
(memq 'red '(red shoes blue socks))