#lang sicp


; timed test
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

; prime test
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (prime? n)
  (= n (smallest-divisor n)))
(define (divides? a b) (= (remainder b a) 0))
(define (square n) (* n n))

; checks primality of consecutive odd integers
; in a specified range
(define (search-for-primes start count)

  (define start-o (convert-to-odd start))
  
  ;(newline)(display "s: ")(display start-o)(display " c: ")(display count)(display " p? ")(display (prime? start))
  
  ; returns whether num is prime
  (define (sfp-itr num)
    (timed-prime-test num)
    (prime? num))

  (cond ((< count 1) false)
        ((sfp-itr start-o) (search-for-primes (+ start-o 2) (- count 1)))
        (else (search-for-primes (+ start-o 2) count))))

(define (convert-to-odd n)
  (if (= 0 (remainder n 2)) (+ n 1) n))

; check for 3 smallest primes larger than
; 1000; 10,000;, 100,000; 1,000,000


(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)

; Yes, the timing data shows that for each multiple of 10
; the initial number increases, the time to calculate if it
; is prime increases by about sqrt(10) = 3 times.
; The data is not exact, but I would say that it loosely supports
; the O(sqrt n) prediction.



