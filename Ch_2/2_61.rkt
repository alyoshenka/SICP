#lang sicp

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

; 2.61

(define (adjoin-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (display set1)(display set2)
         (let ((x1 (car set1))
                    (x2
                     (if (pair? set2)
                         (car set2)
                         set2)))
                (cond ((element-of-set? x2 set1)
                       (adjoin-set set1 (cdr set2)))
                      ((< x1 x2)
                       ;(adjoin-set (cons (car set1) (cons x2 (cdr set1))) (cdr set2)))
                       (cons (car set1) (adjoin-set (cdr set1) set2)))
                      ((< x2 x1)
                       ;(adjoin-set set1 (cdr set2))))))))
                       (adjoin-set (cons x1 (cons x2 (cdr set1))) (cdr set2))))))))
                
                       
        
(display "2.61")(newline)

(adjoin-set (list 1 3 5) (list 2 4 6))
(adjoin-set (list 1 2 3) (list 1 2 3))
(adjoin-set (list 1 2 3) (list 3 2 1))
(adjoin-set (list 3 2 1) (list 1 2 3))
