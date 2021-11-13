#lang sicp


(define dx 0.00001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (lambda (x)
    (/ (+
        (f x)
        (f (- x dx))
        (f (+ x dx)))
       3)))

(define (n-smooth f dx n)
  ((repeated smooth n) f))

(define (f x) (sin x))

(define smooth-f (n-smooth f 0.0001 5))

(smooth-f 0)
(smooth-f 3.1416)