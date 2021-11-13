#lang racket

; p(n, k) = n! / (k! (n - k)! )

; print pascal's triangle
(define (p height)
  (if (= height 0) 0 (line-itr height 0)))

; n!
(define (fac n)
  (define (fac-itr n prod)
    (if (< n 2) prod (fac-itr (- n 1) (* n prod))))
  (fac-itr n 1))

; line iterator
(define (line-itr n r)
  (cell r 0) 
  (display "\n")
  (if (= n r) 0 (line-itr n (+ r 1))))

; display line, from given cell
(define (cell r c)
  (define val (/ (fac r) (* (fac c) (fac (- r c)))))
  (display val) (display " ")
  (if (= r c) 0 (cell r (+ c 1))))


(p  10)

