#lang sicp

; Symbolic Differentiation

; deriv
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))
; variable?
(define (variable? x) (symbol? x))
; same-variable?
(define (same-variable? v1 v2)
(and (variable? v1) (variable? v2) (eq? v1 v2)))
; make-sum
; (define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
; make-product
; (define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
; sum?
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
; addend
(define (addend s) (cadr s))
; augend
(define (augend s) (caddr s))
; product?
(define (product? x) (and (pair? x) (eq? (car x) '*)))
; multiplier
(define (multiplier p) (cadr p))
; multiplicand
(define (multiplicand p) (caddr p))
; =number?
(define (=number? exp num) (and (number? exp) (= exp num)))

; examples
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

; 2.56
; d(u^n)/dx = n*u^(n-1)*du/dx

; x^0 = 1
; x^1 = x

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (exponentiation? exp) (and (pair? exp) (eq? (car exp) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation base exponent)
  (cond ((and (=number? base) (=number? exponent)) (expt base exponent)) ; atom?
        ((and (=number? exponent) (= exponent 0)) 1)
        ((and (=number? exponent) (= exponent 1)) base)
        (else (list '** base exponent))))

; tests
(make-exponentiation 5 2)