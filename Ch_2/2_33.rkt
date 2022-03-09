#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Fill in the missing expressions to complete
; the following definitions of some basic
; list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate
   (lambda (x y)
     (if (null? y) x ; what is the operator for map?
         (p x y)))
   nil
   sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (itr cnt) (+ cnt 1)) 0 sequence))


; tests

(map + (list 1 2 3))
(display "6")
(newline)
(append (list 1 2 3) (list 4 5 6))
(display "(1 2 3 4 5 6)")
(newline)
(length (list 0 0 0))
(display "3")
