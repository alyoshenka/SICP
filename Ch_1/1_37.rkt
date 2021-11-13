#lang sicp

; / n1 (+ d1 (/ n2 (+ d2 (/ n3 d3)))) 

(define (cont-frac n d k)
  (if (not(> k 1))
      (d k)
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter sum i)
    (if (< i 1)
        sum
        (iter (/ (n k) (+ (d k) sum)) (- i 1))))
  (iter 0 k))


; test
(define φ 1.618033)
(define φi (/ 1 φ))

(display φi)(newline)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
(display φi)(newline)
(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)
