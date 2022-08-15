#lang racket

; 2.73

; old derivative helpers
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

; We can regard this program as performing a dispatch on
; the type of the expression to be differentiated. In this
; situation the “type tag” of the datum is the algebraic
; operator symbol (such as +) and the operation being
; performed is deriv
(define (deriv-old exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-old (addend exp) var)
                   (deriv-old (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                    (multiplier exp)
                    (deriv-old (multiplicand exp) var))
                   (make-product
                    (deriv-old (multiplier exp) var)
                    (multiplicand exp))))
        ; ⟨more rules can be added here⟩
        (else (error "unknown expression type: DERIV" exp))))

(display "old derivative rules")(newline)
(deriv-old '(+ x 3) 'x)
(display "'(+ 1 0)")(newline)
(deriv-old '(* x y) 'x)
(display "'(+ (* x 0) (* 1 y))")(newline)
(deriv-old '(* (* x y) (+ x 3)) 'x)
(display "'(+ (* (* x y) (+ 1 0)) (* (+ (* x 0) (* 1 y)) (+ x 3)))")(newline)
(newline)


; We can transform this program into data-directed
; style by rewriting the basic derivative procedure as:
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
         ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; installs the ⟨item⟩ in the table, indexed by the ⟨op⟩ and the ⟨type⟩
; (put ⟨op⟩ ⟨type⟩ ⟨item⟩)

; looks up the ⟨op⟩, ⟨type ⟩ entry in the table and
; returns the item found there.
; If no item is found, get returns false.
; (get ⟨op⟩ ⟨type ⟩)

#|
a. Explain what was done above. Why can’t we assimilate the
predicates number? and variable? into the data-directed dispatch?

First of all, this has abstracted the method of separating the
operator and the operands from the expression. This allows the
new procedure to have different forms for testing against expression
types/operators (sum? prod? etc).

The predicates can't be assimilated because they don't have operators.
A number is just a number, and testing against a variable is just
the same. They are single units, not pairs, so the operator and
operands selectors will fail when treating them as pairs.


--- 
b. Write the procedures for derivatives of sums and products, and
the auxiliary code required to install them in the table used by
the program above.
|#

(define (install-sum-package)
  ;; internal procedures
  (define (make-sum a1 a2) (list '+ a1 a2))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
    
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'sum x))
  (put 'deriv '(+) sum))

(define (make-sum add aug)
  ((get 'sum '+) add aug))

(define (install-product-package)
  ;; internal procedures
  (define (make-product m1 m2) (list '* m1 m2))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (product 

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'product x))
  (put 'deriv '(*) product))

(define (make-product a b)
  ((get 'product '*) a b))

#|

---
c. Choose any additional differentiation rule that you
like, such as the one for exponents (Exercise 2.56), and
install it in this data-directed system.


---
d. In this simple algebraic manipulator the type of an
expression is the algebraic operator that binds it together. Suppose,
however, we indexed the procedures
in the opposite way, so that the dispatch line in deriv
looked like
((get (operator exp) 'deriv) (operands exp) var)
What corresponding changes to the derivative system
are required?

|#