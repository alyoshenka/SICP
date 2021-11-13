#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

; iterative
(define (accumulate-iterative combiner null-value term a next b)
  (define (iter a res)
    (if (> a b) res (iter (next a) (combiner res (term a)))))

  (iter a null-value))

(define (sum a b)
  (define (combiner a b) (+ a b))
  (define (term n) n)
  (define (itr x) (+ x 1))
  
  (accumulate combiner 0 term a itr b))

(define (product a b)
  (define (identity x) x)
  (define (combiner a b) (* a b))
  (define (itr x) (+ x 1))
  
  (accumulate combiner 1 identity a itr b))

; iterative
(define (sum-i a b)
  (define (combiner a b) (+ a b))
  (define (term n) n)
  (define (itr x) (+ x 1))
  
  (accumulate-iterative combiner 0 term a itr b))

(define (product-i a b)
  (define (identity x) x)
  (define (combiner a b) (* a b))
  (define (itr x) (+ x 1))
  
  (accumulate-iterative combiner 1 identity a itr b))



;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))

(display "0 = ")(sum 0 0)
(display "1 = ")(sum 0 1)
(display "3 = ")(sum 0 2)
(display "6 = ")(sum 0 3)
(display "10 = ")(sum 0 4)

(newline)
(display "120 = ")(product 1 5)

(newline)
(display "10 = ")(sum-i 0 4)
(display "120 = ")(product-i 1 5)