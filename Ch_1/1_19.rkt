#lang racket

(define (fib n)
  (fib-iter 1 0 0 1 n 0))

(define (fib-iter a b p q count s)
  (cond ((= count 0) (display "steps: ")(display s)(newline) b)
        ((even? count)
         (fib-iter a b (p- q p) (q- q p) (/ count 2) (+ s 1)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                       (+ (* b p) (* a q))
                       p
                       q
                       (- count 1)
                       (+ s 1)))))

(define (q- q p) (+ (* q q) (* 2 q p)))
(define (p- q p) (+ (* q q) (* p p)))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 27)
(fib (* 2 27))
(fib (* 4 27))