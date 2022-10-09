#lang racket

(define (make-monitored f)
  (let ((cnt 0))
    (define (eval x)
      (begin (set! cnt (+ cnt 1))
             (f x)))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) cnt)
            (else (eval m))))
    dispatch))

(define s (make-monitored sqrt))
(s 100) ; 10
(s 'how-many-calls?) ; 1

                    
