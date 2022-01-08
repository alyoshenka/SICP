#lang sicp

; a, b done. c, d unfinished

#|
Exercise 2.29: A binary mobile consists of two branches,
a left branch and a right branch. Each branch is a rod of
a certain length, from which hangs either a weight or an-
other binary mobile. We can represent a binary mobile us-
ing compound data by constructing it from two branches
(for example, using list):
|#
(define (make-mobile left right)
  (list left right))
#|
A branch is constructed from a length (which must be a
number) together with a structure, which may be either a
number (representing a simple weight) or another mobile:
|#
(define (make-branch length structure)
  (list length structure))

#|
a. Write the corresponding selectors left-branch and
right-branch, which return the branches of a mobile,
and branch-length and branch-structure, which re-
turn the components of a branch.
|#
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length mobile) (car mobile))
(define (branch-structure mobile) (cadr mobile))

#|
b. Using your selectors, define a procedure total-weight
that returns the total weight of a mobile.
|#

(define (is-mobile? item)
    (and
     (pair? item)
     (not (pair? (car item)))
     (pair? (cdr item))
     (null? (cddr item))))

(define (total-weight mobile)

  (define (iter seed sum)
    (if (is-mobile? seed)
        (+ sum (branch-structure seed))
        (iter (left-branch seed) (iter (right-branch seed) sum))))

  (iter mobile 0))

; test
(define length 5)
(define b1 (make-branch length 1))
(define b2 (make-branch length 2))
(define b3 (make-branch length 3))
(define b4 (make-branch length 4))
(define b12 (make-mobile b1 b2))
(define b34 (make-mobile b3 b4))
(define b1234 (make-mobile b12 b34))

; more tests
(display "#t = ")(is-mobile? b1)
(display "#f = ")(is-mobile? b12)
(display "#f = ")(is-mobile? b1234)
(display "#t = ")(is-mobile? b1)
(display "#f = ")(is-mobile? b12)
(display "b1 ")b1
(display "b12 ")b12
(display "b1234 ")b1234
(display "l b12 ")(left-branch b12)
(display "r b12 ")(right-branch b12)

(newline)
; 10
(total-weight b1234)

#|
c. A mobile is said to be balanced if the torque applied by
its top-left branch is equal to that applied by its top-
right branch (that is, if the length of the left rod mul-
tiplied by the weight hanging from that rod is equal
to the corresponding product for the right side) and if
each of the submobiles hanging off its branches is bal-
anced. Design a predicate that tests whether a binary
mobile is balanced.
|#

; must be a base mobile (no children)
(define (torque-base mobile) (* (branch-length mobile) (branch-structure mobile)))
; total torque
(define (torque-total mobile)

  (define (iter seed sum)
    (if (is-mobile? seed)
        (+ sum (* (branch-length seed) (branch-structure seed)))
        (iter (left-branch seed) (iter (right-branch seed) sum))))

  (iter mobile 0))

(define (balanced? mobile)
  ; check the top level branch, then work down
  ; (this causes a lot of redundant calculations)
  

  ; get the "next" branch down
  (define (get-left seed)
    (if
     (is-mobile? seed)
      seed
      (left-branch seed)))
  (define (get-right seed)
    (if
     (is-mobile? seed)
      seed
      (right-branch seed)))

  (define (branch-balanced? seed)
    (newline)
    (display "branch-balanced?")(newline)
    (display "seed: ")(display seed)(newline)
    (display "l torque: ")(display (torque-total (get-left seed)))(newline)
    (display "r torque: ")(display (torque-total (get-right seed)))(newline)
    (newline)
    (if
     (is-mobile? seed)
     ; idfk
     ; ((= (torque-base (left-branch seed)) (torque-base (right-branch seed))))
     #t
     (= (torque-total (get-left seed)) (torque-total (get-right seed)))))

  ; check that all branches are balanced

  ; what to do with "unbalanced" branches?
  (define (iter seed)
    (display "seed: ")(display seed)(newline)
    (display "balanced? ")(display (branch-balanced? seed))
    (newline)
    ; exit condition: both branches are mobiles
    (if
     (and (is-mobile? (left-branch seed)) (is-mobile? (right-branch seed)))
     ; check torque of both "leaves"
     (branch-balanced? seed)
     ; otherwise check current tree, then next level down
     (and (branch-balanced? seed) (iter (get-left seed)) (iter (get-right seed)))))

  (iter mobile))
     
; #f
(balanced? b1234)

; test out a balanced mobile

(define a1 (make-mobile 5 2))
(define a2 (make-mobile 2 5))
(define a3 (make-mobile a1 a2))
(balanced? a3) ; #t

(define a4 (make-mobile 1 5))
(define a5 (make-mobile 5 1))
(define a45 (make-mobile a4 a5))
(define a6 (make-mobile a1 a45))
(balanced? a6) ; #t

(display "\nstop here\n\n")

; this just needs better numbers
(define c1 (make-branch 2 4))
(define c2 (make-branch 2 2))
(define c12 (make-branch c1 c2))

(define c3 (make-branch 2 1))
(define c4 (make-branch 1 2))
(define c34 (make-branch c3 c4))

(define c5 (make-branch 6 2))
(define c125 (make-branch c12 c5))

(define c6 (make-branch 2 4))
(define c346 (make-branch c34 c6))

(define b-mobile (make-branch c125 c346))

(balanced? b-mobile)

#|
d. Suppose we change the representation of mobiles so
that the constructors are
|#
(define (make-mobile2 left right) (cons left right))
(define (make-branch2 length structure)
  (cons length structure))
#|
How much do you need to change your programs to
convert to the new representation?
|#