#lang sicp

; 2.18
; not the most elegant solution, but it works

(define (length-iter items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (reverse items)
  (let
      ((len (length-iter items)))
    (define (add-item i)
      (if (< i 0)
          nil
          (cons (list-ref items i) (add-item (- i 1)))))
    (add-item (- len 1))))

; test
(reverse (list 1 2 3 4 5))
(reverse (list 5))
(reverse (list 2 3))