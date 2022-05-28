#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; converts an ordered list to a balanced binary tree
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; a pair (formed with cons) whose car is the constructed tree
; and whose cdr is the list of elements not included in the tree
(define (partial-tree elts n)
  (display "partial tree called")(newline)
  ;(newline)
  ;(display "partial-tree ")(display elts)(display " n: ")(display n)(newline)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        ;(display "left-size: ")(display left-size)(newline)
        ;(display "getting left result...")(newline)
        (let ((left-result
               (partial-tree elts left-size)))
          ;(display "left-result: ")(display left-result)(newline)
          ;(display "getting left tree...")(newline)
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            ;(display "left-tree: ")(display left-tree)(newline)
            ;(display "non-left-elts: ")(display non-left-elts)(newline)
            ;(display "right-size: ")(display right-size)(newline)
            ;(display "getting right result...")(newline)
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              ;(display "this-entry: ")(display this-entry)(newline)
              ;(display "getting right tree...")(newline)
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                ;(display "right-tree: ")(display right-tree)(newline)
                ;(display "remaining-elts: ")(display remaining-elts)(newline)
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

; 2.64

#|
a. Write a short paragraph explaining as clearly as you
can how partial-tree works. Draw the tree produced
by list->tree for the list (1 3 5 7 9 11).

   5
 /   \
1     9
 \   / \
  3 7  11

partial-tree recursively constructs sub-trees from the left and
then cons's them together. The size of the left tree given the element
size is (n-1)/2 because it needs half of the elements on one side
(minus the 1 for the parent). This means that elements will be skewed
to the right side if the total number is not exactly 2^c-1.
To construct the left tree, it constructs a subtree made of the first
(n-1)/2 elements, which returns as a pair of this tree and the remaining
elements. Then, a tree is made with the next of the remaining elements
as the head, the created branch as the left, and constructs the right
tree with size (n - (left size + 1)). With n being the number of elements in the tree, these sum to n.
This function recursively splits the tree into halves, then constructs the
subtrees of that length. This works because it takes out successively
smaller "chunks" of the initial elements.

b. What is the order of growth in the number of steps required by list->tree to convert a list of n elements?

O(N)
|#

(list->tree (list 1 3 5 7 9 11))
(list->tree (list 1))
(list->tree (list 1 3 5))
(list->tree (list 1 3 5 7 9 11 13))
(list->tree (list 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29))