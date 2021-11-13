#lang sicp


(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (repeated f n)
  (define (iter k)
    (if (< k 2)
        (identity f)
        (compose f (iter (- k 1)))))
   (iter n))

625
((repeated square 2) 5)