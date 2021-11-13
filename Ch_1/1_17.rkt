#lang racket

(define (double n) (* n 2))

(define (halve n) (/ n 2))

; n1 * n2
(define (mult n1 n2) (mult-itr n1 n2 1))

(define (mult-itr b n s)
  (cond
    ((= n 0) (display "s: ")(display s)(display "\n") 0)
    ((= n 2) (display "s: ")(display s)(display "\n") (double b))
    ((= (remainder n 2) 0) (double (mult-itr b (halve n) (+ s 1))))
    (else (+ b (mult-itr b (- n 1) (+ s 1))))))


; make sure to test 0 and negatives!

(mult 4 5)
(mult 3 37)
(* 3 37)
(mult 3 74)
(* 3 74)