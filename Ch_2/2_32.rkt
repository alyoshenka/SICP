#lang sicp

#|
Exercise 2.32: We can represent a set as a list of distinct
elements, and we can represent the set of all subsets of the
set as a list of lists. For example, if the set is (1 2 3), then
the set of all subsets is (() (3) (2) (2 3) (1) (1 3)
(1 2) (1 2 3)). Complete the following definition of a
procedure that generates the set of subsets of a set and give
a clear explanation of why it works:
|#

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (cur-tree)
                        (append (list (car s)) cur-tree))
                      rest)))))

(display "(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))")(newline)
(subsets (list 1 2 3))

#|
The map procedure applies some transformatoin to each element in a list.
In this case, that procedure is to return all lists that can be created
from the current item in the list onward. For example, (2 3) -> (2) (3) ().
As the set is traveled through, it gets smaller so items are not repeated.
|#