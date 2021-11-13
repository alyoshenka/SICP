#lang sicp


; generic sum function
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

; integration of f from a to b using simpson's method
(define (simpsons-integral f a b n)
  
  (define (term k)
    (define (mult i)
      (cond ((or (= 0 i) (= n i)) 1)
            ((= 0 (remainder i 2)) 2)
            (else 4)))
    
    (* (mult k) (f (+ a (* k (/ (- b a) n))))))
  
  (define (next n) (+ n 1))

  (* (/ (/ (- b a) n) 3) (sum term 0 next n)))

(display "0.25 ~ ")(simpsons-integral cube 0 1.0 100)
(display "0.25 ~ ")(simpsons-integral cube 0 1.0 1000)
  
    
  