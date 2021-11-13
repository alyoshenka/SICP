#lang racket

(define (f-rec n)
  (if (< n 3) n
       (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

(f-rec 0)
(f-rec 1)
(f-rec 2)
(f-rec 3)
(f-rec 4)
(f-rec 5)
(f-rec 6)
(f-rec 7)
(f-rec 8)
(f-rec 9)


(define (f-itr n)
  (if (< n 3) n
      (f-itr-in n 4 2 1)))

(define (f-itr-in n prev1 prev2 prev3) 
  (cond ((= n 3) prev1)
         ((< n 4) 4)
      (else (f-itr-in (- n 1 )(+ prev1 (* 2 prev2) (* 3 prev3)) prev1 prev2))))

(f-itr 0)
(f-itr 1)
(f-itr 2)
(f-itr 3)
(f-itr 4)
(f-itr 5)
(f-itr 6)
(f-itr 7)
(f-itr 8)
(f-itr 9)