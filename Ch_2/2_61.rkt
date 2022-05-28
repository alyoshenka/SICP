#lang racket

(provide element-of-set?)
(provide intersection-set)
(provide adjoin-set)
(provide union-set)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; O(N)
; At each step, one element is removed from one (or both) sets.
; This means that the total number of steps required cannot
; exceed the sum of the lengths of both sets. This is dependant
; on N.
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

; 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else             
         (let ((x1 (car set1))
                    (x2
                     (if (pair? set2)
                         (car set2)
                         set2)))
                (cond ((element-of-set? x2 set1)
                       (union-set set1 (cdr set2)))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      ((< x2 x1)
                       (cons x2 (union-set (cons x1 (cdr set1)) (cdr set2)))))))))
                
                       
        
(display "2.62")(newline)

(union-set (list 1 3 5) (list 2 4 6))
(union-set (list 1 5 6) (list 2 3 4))
(union-set (list 1 2 3) (list 1 2 3))

(define s1 (list 1 2 3))
(define s2 (list 4 5 6))

(display "#t")(newline)
(element-of-set? 3 (union-set s1 s2))
(or (element-of-set? 3 s1) (element-of-set? 3 s2))

(display "#t")(newline)
(element-of-set? 4 (union-set s1 s2))
(or (element-of-set? 4 s1) (element-of-set? 4 s2))

(display "#f")(newline)
(element-of-set? 7 (union-set s1 s2))
(or (element-of-set? 7 s1) (element-of-set? 7 s2))

; 2.62

(define (adjoin-set x set)
  (cond
    ((null? set) (list x))
    ((not (pair? set)) (list set x))
    (else 
     (let ((x2 (car set)))
       (cond ((= x x2) set)
             ((< x x2) (cons x set))
             ((< x2 x) (cons x2 (adjoin-set x (cdr set)))))))))

(display "2.61")(newline)

(adjoin-set 1 (list 1 2 3))
(adjoin-set 1 (list 2 3 4))
(adjoin-set 4 (list 1 2 3))