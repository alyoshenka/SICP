#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

#|
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
|#

; Redefine count-leaves from as an accumulation:
(define (count-leaves t)
  (accumulate
   +
   0
   (map
    (lambda (t)
      (cond
        ((null? t) 0)
        ((pair? t) (count-leaves t))
        (else 1)))          
      t)
    ))


(display "4")(newline)
(count-leaves (list 1 2 (list 3 4)))
