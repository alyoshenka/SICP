#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; tests
(define s1 (list 1 2 3))
(define s2 (list 4 5 6))

(display "#t")(newline)
(element-of-set? 6 (adjoin-set 6 s1))

(display "#f")(newline)
(element-of-set? 0 '())

; 2.59

; for every value in s1
;   check if it's in s2
;     if so, add to resulting set
(define (union-set set1 set2)
  (cond ((null? set2)
         set1)
        ((not (element-of-set? (car set2) set1))
         (cons (car s2) (union-set set1 (cdr set2))))
        (else
         (union-set set1 (cdr set2)))))
  


(display "2.59")(newline)

(display "#t")(newline)
(element-of-set? 3 (union-set s1 s2))
(or (element-of-set? 3 s1) (element-of-set? 3 s2))

(display "#t")(newline)
(element-of-set? 4 (union-set s1 s2))
(or (element-of-set? 4 s1) (element-of-set? 4 s2))

(display "#f")(newline)
(element-of-set? 7 (union-set s1 s2))
(or (element-of-set? 7 s1) (element-of-set? 7 s2))

; 2.60

; Duplicates or not, it iterates through the set
; and returns #t on the first hit
; O(N) in the worst case, where the element has only
;  one instance and is at the end of the list representing
;  the set. Allowing duplicates does potentially increase N.
(define element-of-set-dup? element-of-set?)

; Just has to add one to the end
; O(1)
(define (adjoin-set-dup x set) (cons x set))

; The "lazy" implementation just concatenates the sets
; O(N), though this does potentially greatly increase the set length
;  O(1) potentially, depending on Scheme performance of 'append'
(define union-set-dup append)

; This still needs to create a set by iterating over one set for
; initial values, and the other set to check if they match. Allowing
; duplicates doesn't change this.
; O(N^2)
(define intersection-set-dup intersection-set)

#|
Basically, anything that makes the set bigger is more performant
(adjoin, union) and anything that makes it smaller (intersection)
is less performant. element is slower, though not necessarily less
performant, because it is an O(N) operation and allowing duplicate elements potentially increases N.

Use this representation for operations that depend on creating sets,
not for ones that check whether that set contains an element.

Creates more memory!
|#

(define s1-dup (list 1 2 3 1 2 3))
(define s2-dup (list 4 4 5 5 6 6))

(display "2.60")(newline)

(display "#t")(newline)
(element-of-set-dup? 3 (union-set-dup s1-dup s2-dup))
(or (element-of-set-dup? 3 s1-dup) (element-of-set-dup? 3 s2-dup))

(display "#t")(newline)
(element-of-set-dup? 4 (union-set-dup s1-dup s2-dup))
(or (element-of-set-dup? 4 s1-dup) (element-of-set-dup? 4 s2-dup))

(display "#f")(newline)
(element-of-set-dup? 7 (union-set-dup s1-dup s2-dup))
(or (element-of-set-dup? 7 s1-dup) (element-of-set-dup? 7 s2-dup))