#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iterative term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* (term a) res))))
  (iter a 1))

(define (factorial n)
  (define (identity x) x)
  (define (itr x) (+ 1 x))

  (product identity 1 itr n))

(display "5040 = ")(factorial 7)

; calculate pi (pi/4 * 4 = pi)

(define (pi4-term i)
  (define (num i) ; numerator
    (if (= 0 (remainder i 2)) (+ i 2) (+ i 3)))
  (define (den i) ; denominator
     (if (= 0 (remainder i 2)) (+ i 3) (+ i 2)))
  (/ (num i) (den i)))

(define (pi4-next n) (+ n 1))

(display "3.14... ~ ")(* 4 (product pi4-term 0.0 pi4-next 10))
(display "3.14... ~ ")(* 4 (product pi4-term 0.0 pi4-next 100))
(display "3.14... ~ ")(* 4 (product pi4-term 0.0 pi4-next 1000))
(display "3.14... ~ ")(* 4 (product pi4-term 0.0 pi4-next 10000))

(newline)
(display "3.14... ~ ")(* 4 (product-iterative pi4-term 0.0 pi4-next 10))
(display "3.14... ~ ")(* 4 (product-iterative pi4-term 0.0 pi4-next 100))
(display "3.14... ~ ")(* 4 (product-iterative pi4-term 0.0 pi4-next 1000))
(display "3.14... ~ ")(* 4 (product-iterative pi4-term 0.0 pi4-next 10000))