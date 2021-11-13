#lang sicp

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;  first-denomination
(define (first-denomination _list) (car _list))

; except-first-denomination
(define (except-first-denomination _list) (cdr _list))

; no-more?
(define (no-more? _list) (null? _list))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))



; original operation
(define (count-change amount) (cc_old amount 5))
(define (cc_old amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc_old amount
                     (- kinds-of-coins 1))
                 (cc_old (- amount
                        (first-denomination_old
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination_old kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 10)
        ((= kinds-of-coins 3) 5)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; test
(cc 100 us-coins)
(count-change 100)
(= (cc 100 us-coins) (count-change 100))
(cc 100 uk-coins)

(define us-coins-reorganized (list 25 50 1 10 5))
(cc 100 us-coins-reorganized)

#|
Does the order of the list coin-values affect the answer produced by cc?
Why or why not?

no
The order of the input list would affect the order of the output list
if different combinations were shown, however all values and combinations
of coins will still get checked regardless of order. The same behavior
is shown in the old list, the 5 and 10 have been swapped. 
|#