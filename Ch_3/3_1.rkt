#lang racket

(define (make-accumulator init)
  (let ((acc init))
    (lambda (n)
      (begin
        (set! acc (+ acc n))
        acc))))

(define A (make-accumulator 5))

(A 10) ; 15
(A 10) ; 25