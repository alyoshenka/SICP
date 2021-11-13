#lang sicp

(define (cont-frac n d k)
  (if (not(> k 1))
      (d k)
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter sum i)
    (if (< i 1)
        sum
        (iter (/ (n i) (+ (d i) sum)) (- i 1))))
  (iter 0 k))

(define e 2.71828)
(display e)(newline)
(+ 2 (cont-frac-iter
      (lambda (n) 1.0)      
      (lambda (d)
        (if (= 0 (remainder (+ 1 d) 3))
            (* 2 (/ (+ 1 d) 3)) 
            1))
     9))