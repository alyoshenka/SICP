#lang sicp

#| 
Exercise 2.27: Modify your reverse procedure of Exercise
2.18 to produce a deep-reverse procedure that takes a list
as argument and returns as its value the list with its ele-
ments reversed and with all sublists deep-reversed as well.
|#

(define (length-iter items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ 1 count))))
  (iter items 0))

(define (reverse items)
  (let
      ((len (length-iter items)))
    (define (add-item i)
      (if (< i 0)
          nil
          (cons (list-ref items i) (add-item (- i 1)))))
    (add-item (- len 1))))

(define (deep-reverse items)
  (define (add-item i)
    (if (< i 0)
        nil
        (if (pair? items)
            ; if it's a pair, reverse it
            (cons (deep-reverse (list-ref items i)) (add-item (- i 1)))
            ; otherwise proceed as usual
            (cons (if (pair? items) (list-ref items i) items) (add-item (- i 1))))))
  ; if an item is reversed, just return the item
  (if (pair? items) (add-item (- (if (pair? items) (length-iter items) 1) 1)) items))

; For example,

(define x (list (list 1 2) (list 3 4)))
x
(display "((1 2) (3 4))\n")
(newline)
(reverse x)
(display "((3 4) (1 2))\n")
(newline)
(deep-reverse x)
(display "((4 3) (2 1))\n")
(newline)