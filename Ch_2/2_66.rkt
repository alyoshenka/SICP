#lang racket

(require "2_63.rkt")
(newline)(display "-- 2.66 --")(newline)

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (entry set-of-records))
         (entry set-of-records))
        ((< given-key (entry set-of-records))
         (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))

(define t1 (make-tree 2 (make-tree 1 '() '()) (make-tree 3 '() '())))

(display "1 = ")(lookup 1 t1)
(display "#f = ")(lookup 4 t1)