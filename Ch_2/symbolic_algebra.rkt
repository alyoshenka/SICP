#lang racket

(require "./operations_table.rkt")
(require "./generic_arithmetic_package.rkt")

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  ; 2.87
  ; change to just deal with term list
  (define (=zero?-list poly)
    (if (empty-termlist? (term-list poly))
        #t
        (and (=zero? (car (first-term (term-list poly))))
             (=zero?-list (rest-terms (term-list poly))))))
  ; ---
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  ; 2.87
  (put '=zero? '(polynomial)
       (lambda (poly) (=zero?-list poly)))
  ; ---
  'done)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(newline)(display "Symbolic Algebra")(newline)

(newline)(display "Installing packages")(newline)
(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)

(make-scheme-number 3)

(define a (make-polynomial 'x '()))
(define b (make-polynomial 'x (list (list
                                     (make-scheme-number 4)
                                     (make-scheme-number 3))
                                    (list
                                     (make-scheme-number 2)
                                     (make-scheme-number 1))
                                    (list
                                     (make-scheme-number 0)
                                     (make-scheme-number 2)))))
(define c (make-polynomial 'x (list (list
                                     (make-scheme-number 3)
                                     (make-scheme-number 0))
                                    (list
                                     (make-scheme-number 2)
                                     (make-rational 0 4))
                                    (list
                                     (make-scheme-number 1)
                                     (make-scheme-number 0)))))

(newline)(display "2.87")(newline)

(=zero? (make-scheme-number 0))
(=zero? (make-scheme-number 4))

(define (zero-test poly expected)
  (display "=zero?: ")
  (display (=zero? poly))
  (display " (")
  (display expected)
  (display ")")
  (newline))

(zero-test a #t)
(zero-test b #f)
(zero-test c #t)