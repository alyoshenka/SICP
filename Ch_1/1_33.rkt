#lang sicp

; a ok, b unfinished

(define (filtered-accumulate combiner null-value term a next b filter)
  0)

; (filter num) -> #t/#f

(define (accumulate combiner null-value term a next b filter)
  (define (use-filter pass fail)
    (if (filter pass) pass fail))
  
  (if (> a b)
      null-value
      (combiner (term (use-filter a null-value)) (accumulate combiner null-value term (next a) next b filter))))


; helper functions

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

(define (gcd a b)
(if (= b 0)
    a
    (gcd b (remainder a b))))

(define (incr x) (+ x 1))

(define (relatively-prime? i n) (= 1 (gcd i n)))


; test

(+ (square 2) (square 3) (square 5))
(accumulate + 0 square 2 incr 5 prime?)

(accumulate * 1 identity 0 incr 5 relatively-prime?)