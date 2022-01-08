#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length-recr items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))


(define (length-iter items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
                        
                        

; 2.17
(define (last-pair list1)
  (if (null? (cdr list1))
      (car list1)
      (last-pair (cdr list1))))

; test
(= 5 (last-pair (list 1 2 3 4 5)))

; 2.18
; not the most elegant solution, but it works
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

