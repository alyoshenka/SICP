#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

#| e procedure accumulate-n is similar to accumulate except that
it takes as its third argument a sequence of sequences, which are
all assumed to have the same number of elements. It applies the designated accumulation procedure to combine all the first elements of the sequences,
all the second elements of the sequences, and so on,
and returns a sequence of the results
|#
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))


; 2.37

; (map proc items)
#|
is more general map takes a procedure of n arguments, together with n lists,
and applies the procedure to all the first elements of the lists,
all the second elements of the lists, and so on, returning a list of the results.
For example:

(map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
(741 852 963)

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))
(9 12 15)
|#


(define (matrix-*-vector m v)
  (map
   ; all the "rows" of m will be passed in individually (w)
   
   ; code here
   (lambda (w) (dot-product v w))
   ; end code
   m))

(define (transpose mat)
  (accumulate-n
   ; op
   cons
   ; init
   nil
   ; seq
   mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     ; all the rows of m will be passed in individually
     ; multiplying the row by the transposed matrix
     ; results in the sequence corresponding to that
     ; row of the output
     ; map then constructs a list of these outputs
     
     ; proc
     (lambda (w) (matrix-*-vector cols w))
     ; end code
     m)))


; tests

(display "(1 -3)")(newline)
(matrix-*-vector (list (list 1 -1 2) (list 0 -3 1)) (list 2 1 0))
(newline)

(display "((1 4) (2 5) (3 6))")(newline)
(transpose (list (list 1 2 3) (list 4 5 6)))
(newline)

(display "((0 -10) (-3 -1))")(newline)
(matrix-*-matrix
 (list
  (list 0 4 -2)
  (list -4 -3 0))
 (list
  (list 0 1)
  (list 1 -1)
  (list 2 3)))
(newline)