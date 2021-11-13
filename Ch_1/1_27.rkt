#lang sicp

(define (mod a b) (remainder a b))

(define (square n) (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (carmichael-demo n)
  (define (carmichael-demo-itr n a)
    (cond ((not (< a n)) #t)
          ((= (expmod a n n) (mod a n)) (carmichael-demo-itr n (+ a 1)))
          (else #f)))
  (carmichael-demo-itr n 1))


(carmichael-demo 241) ; #t
(newline)
(carmichael-demo 562) ; #f
(newline)

; not prime, but should return #t
(carmichael-demo 561) ; 3 * 187
(carmichael-demo 1105)
(carmichael-demo 1729)
(carmichael-demo 2465)
(carmichael-demo 2821)
(carmichael-demo 6601)