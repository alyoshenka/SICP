#lang sicp

(define (cont-frac-iter n d k)
  (define (iter sum i)
    (if (< i 1)
        sum
        (iter (/ (n i) (+ (d i) sum)) (- i 1))))
  (iter 0 k))

(define (tan-cf x k)
  (cont-frac-iter
   (lambda (n)
     (if (= n 1)
          x
          (- (* x x))))
   (lambda (d) (- (* 2 d) 1))
   k))

0.0
(tan-cf 0.0 5)
(newline)
(- 2.185040)
(tan-cf 2.0 10)
(newline)
0.29101
(tan-cf (- 6.0) 13)