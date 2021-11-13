#lang sicp

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (next num)
  (if (= num 2) 3 (+ num 2)))

(define (square n) (* n n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))


(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (prime? n)
  (= n (smallest-divisor n)))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)

(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)

(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)

(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

; This optimized version runs close to twice as fast,
; but not quite.
; I put it at about 1.7 (sqrt 3) times faster.
; It is not twice as fast because while it does decrease
; the number of test steps, it does add another method call