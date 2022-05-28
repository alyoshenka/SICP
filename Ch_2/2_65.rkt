#lang racket

(require racket/include)
(require "2_63.rkt")
;(require "2_64.rkt")
(newline)(display "-- 2.65 --")(newline)


(define (union-set tree1 tree2)
  (cond ((null? tree2) tree1)
        (else (let ((left (union-set tree1 (left-branch tree2))))
                (let ((tree (adjoin-set (entry tree2) tree1)))
                  (union-set tree (right-branch tree2)))))))




(define (intersection-set tree1 tree2)
  (cond ((null? tree2) tree1)
        ((element-of-set? (entry tree2) tree1)
         (let ((left (intersection-set tree1 (left-branch tree2))))
                (let ((tree (adjoin-set (entry tree2) tree1)))
                  (intersection-set tree (right-branch tree2)))))
        (else (let ((left (intersection-set tree1 (left-branch tree2))))
                (intersection-set left (right-branch tree2))))))




(define t1 (make-tree 2 (make-tree 1 '() '()) (make-tree 3 '() '())))
(define t2 (make-tree 3 (make-tree 2 '() '()) (make-tree 4 '() '())))

(display t1)(newline)
(display t2)(newline)

(display "- union")(newline)
(union-set t1 t2)
(display "- intersection")(newline)
(intersection-set t1 t2)

(display "- tests")(newline)
; "definition" tests
(display "#t = ")
(display (element-of-set? 4 (adjoin-set 4 t1)))
(newline)
(display (element-of-set? 4 (union-set t1 t2)))
(display " = ")
(display (or (element-of-set? 4 t1) (element-of-set? 4 t2)))
(newline)
(display (element-of-set? 5 (union-set t1 t2)))
(display " = ")
(display (or (element-of-set? 5 t1) (element-of-set? 5 t2)))
(newline)
(display "#f = ")(display (element-of-set? 1 '()))
(newline)