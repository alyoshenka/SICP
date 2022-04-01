#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square x) (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define m 6)

(display "pairs of 6")(newline)
(accumulate
 append nil (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 m)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(display "prime sums of 6")(newline)
(prime-sum-pairs 6)

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

; 2.40
(newline)
(display "2.40")(newline)

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list j i))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
       

(display "(unique-pairs 4)")(newline)
(display "((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))")(newline)
(unique-pairs 4)


; 2.41
(newline)
(display "2.41")(newline)

(define (less-sum-triples n s)
  (map make-triple-sum
       (filter
        (lambda (trip) (= s (+ (car trip) (cadr trip) (caddr trip))))
        (unique-triples n))))

(define (unique-triples n)
  (flatmap
   (lambda (l)
     (map (lambda (k) (list (car l) (cadr l) k))
          (enumerate-interval (+ (cadr l) 1) n)))
   (unique-pairs n)))

(define (make-triple-sum trip)
  (let ((a (car trip)) (b (cadr trip)) (c (caddr trip)))
    (list a b c (+ a b c))))
  

(newline)
(display "unique-triples 4")(newline)
(unique-triples 4)

(newline)
(display "(1 2 3 6)")(newline)
(make-triple-sum (list 1 2 3))

(newline)
(display "less-sum-triples 5 8")(newline)
(less-sum-triples 5 8)

; 2.42
(newline)
(display "2.42: eight-queens puzzle")(newline)

; returns a sequence of all solutions to the problem
; of placing n queens on an n * n chessboard
(define (queens board-size)
  ; returns the sequence of all ways to place queens
  ; in the first k columns of the board
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          ; rest-of-queens: a way to place k-1 queens in the
          ; first k-1 columns
          (lambda (rest-of-queens)
            ; new-row: proposed row in which to place the queen
            ; for the kth column
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; complete the program by implementing the representation for
; sets of board positions

; represents an empty set of positions
(define empty-board nil)

; determines for a set of positions whether the queen
; in the kth column is safe with respect to the others
(define (safe? col positions)
  ; only placing one queen in a column at a time,
  ; so don't need to check column

  ; check for queens in the same row
  ; check for queens in the same diagonal

  ; if the difference in the rows is the same as the
  ; difference in the cols, they are in the same diag

  ; find the queen that is in the given column
  ; this queen will always be at index (col - 1)
  ; due to how the function is called

  ; get the set of all other queens (not in same column)

  (define (attacks? q1 q2)
    (or (= (get-row q1) (get-row q2))
        (= (abs (- (get-row q1) (get-row q2)))
           (abs (- (get-col q1) (get-col q2))))))

  ; check if queen can attack any queen in board
  (define (iter queen board)
    (or (null? board)
        (and (not (attacks? queen (car board)))
             (iter queen (cdr board)))))

  (let ((new-queen (list-ref positions (- col 1))))
    (let ((other-queens
           (filter
            (lambda (pos)
              (not (= col (get-col pos))))
            positions)))

      (iter new-queen other-queens))))
  

; append takes two or more lists and constructs a new list
; with all of their elements

; adjoins a new row-column position to a set of positions
(define (adjoin-position row col positions)
  (append positions (list (make-pos row col))))

(define (make-pos row col) (cons row col))
(define (get-row pos) (car pos))
(define (get-col pos) (cdr pos))


(newline)
(display "4 queens")(newline)
(queens 4)
(display "92")(newline)
(length (queens 8))

; 2.43
(newline)
(display "2.43")(newline)

(define (long-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

#|
Explain why this interchange makes the program run slowly.
Estimate how long it will take Louis’s program to solve the
eight-queens puzzle, assuming that the program in Exercise
2.42 solves the puzzle in time T.

T*k^2
The original procedure adds a new column entry once for every row
The new procedure adds a new row for every entry in rest-of-queens,
however every iteration it must calculate the existing queens
from 1 - k

Official answer (had to look up):
T^k
In the original solution, queen-cols is called once for each column
in the board. This is an expensive procedure to call, since it
generates the sequence of all possible ways to place k queens in k
columns. By moving queen-cols so it gets called by flatmap, we're
transforming a linear recursive process to a tree-recursive process.
The flatmap procedure is called for each row of the kth column, so the
new procedure is generating all the possible solutions for the first
k - 1 columns for each one of these rows.
Tree-recursive processes grow exponentially. It it takes time T to
execute the original version of queens for a given board size, we can
expect the new version to take roughly T^k time to execute.
|#