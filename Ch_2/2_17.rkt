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

