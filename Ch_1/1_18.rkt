#lang racket

; iterative process of multiplication
; using a logarithmic number of steps

(define (double n) (* n 2))

(define (halve n) (/ n 2))

; n1 * n2
(define (fast-mult n1 n2) (fast-mult-itr n1 n2 0 n1 0))

(define (fast-mult-itr b n t m s)
  (cond
    ((= n 0) 0) ; * 0
    ((= n 1) b) ; * b
    ((= n 2) (display "steps: ")(display s)(newline)(+ t (double m))) ; something?? last step
    ((= (remainder n 2) 0) (fast-mult-itr b (halve n) t (double m) (+ s 1)))
    (else (fast-mult-itr b (- n 1) (+ t m) m (+ s 1)))))

; make sure to test 0 and negatives!
; integers are positive, just test 0

(fast-mult 5 0)
(fast-mult 5 1)
(fast-mult 5 2)
(fast-mult 5 3)

(display "\n")
(* 5 91)
(fast-mult 5 91)

(display "\n")
(* 3 765)
(fast-mult 3 765)
(fast-mult 3 (* 2 765))
(fast-mult 3 (* 2 2 765))