#lang racket

(provide element-of-set?)
(provide adjoin-set)
(provide entry left-branch right-branch make-tree)

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

; 2.63

(define (tree->list-1 tree)
  (display "tree list: ")(display tree)(newline)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (display "copy-to-list tree: ")(display tree)(display " result: ")(display result-list)(newline)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define t1 (make-tree 7 (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '())) (make-tree 9 '() (make-tree 11 '()'()))))
(define t2 (make-tree 3 (make-tree 1 '() '()) (make-tree 7 (make-tree 5 '() '()) (make-tree 9 '() (make-tree 11 '() '())))))
(define t3 (make-tree 5 (make-tree 3 (make-tree 1 '() '()) '()) (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))
(define t4 (make-tree 1 '() (make-tree 2 '() (make-tree 3 '() (make-tree 4 '() (make-tree 5 '() (make-tree 6 '() (make-tree 7 '() '()))))))))

#|
a) They look to be the same.

b) The first solution is slower
The append procedure makes the first procedure run in O(n log n), while the second is O(n)
n log n > n
|#

(define time1-start (current-inexact-milliseconds))

;(tree->list-1 t1)
(tree->list-1 t2)
;(tree->list-1 t3)
;(tree->list-1 t4)

(define time1-end (current-inexact-milliseconds))
(display (- time1-end time1-start))(newline)

(define time2-start (current-inexact-milliseconds))

;(tree->list-2 t1)
(tree->list-2 t2)
;(tree->list-2 t3)
;(tree->list-2 t4)

(define time2-end (current-inexact-milliseconds))
(display (- time2-end time2-start))(newline)
