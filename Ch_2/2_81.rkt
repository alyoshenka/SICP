#lang racket

(require "./operations_table.rkt")

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (apply-generic op . args)
  ;(display op)(display " ")(display args)(newline)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                ;(display "a1: ")(display a1)(newline)
                ;(display "a2: ")(display a2)(newline)
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         ;(display "found mapping ")(display type1)
                         ;(display " to ")(display type2)(newline)
                         ;(display t1->t2)(newline)
                         ;(display (t1->t2 a1))(newline)
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; generic arithmetic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; 2.79
(define (equ? x y) (apply-generic 'equ? x y))
; ---
; 2.80
(define (=zero? x) (apply-generic '=zero? x))
; ---

; ---
; scheme-number (basic numbers)
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  ; 2.79
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (eq? x y)))
  ; ---
  ; 2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))
  ; ---
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
; ---

; rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ; 2.79
  (define (equ? a b)
    (and
     (eq? (numer a) (numer b))
     (eq? (denom a) (denom b))))
  ; ---
  ; 2.80
  (define (=zero? r)
    (= 0 (numer r)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d)
         ;(display "n: ")(display n)(newline)
         (tag (make-rat n d))))
  ; 2.79
  (put 'equ? '(rational rational) equ?)
  ; ---
  ; 2.80
  (put '=zero? '(rational) =zero?)
  'done)
(define (make-rational n d)
  ;(display "making rational")(newline)
  ((get 'make 'rational) n d))
; ---

; rectangular complex numbers
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude-rectangular z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ; 2.79
  (define (equ? a b)
    (and
     (eq? (real-part a) (real-part b))
     (eq? (imag-part a) (imag-part b))))
  ; ---
  ; 2.80
  (define (=zero? r)
    (= 0 (magnitude-rectangular r)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude-rectangular)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ; 2.79
  (put 'equ? '(rectangular rectangular) equ?)
  ; ---
  ; 2.80
  (put '=zero? '(rectangular) =zero?)
  'done)
; ---

; polar complex numbers
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude-polar z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude-polar z) (cos (angle z))))
  (define (imag-part z) (* (magnitude-polar z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ; 2.79
  (define (equ? x y)
    (and
     (= (magnitude-polar x) (magnitude-polar y))
     (= (angle x) (angle y))))
  ; ---
  ; 2.80
  (define (=zero? p)
    (= 0 (magnitude-polar p)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ; 2.79
  (put 'equ? '(polar polar)
       (lambda (x y) (equ? x y)))
  ; ---
  ; 2.80
  (put '=zero? '(polar)
       (lambda (p) (=zero? p)))
  ; ---
  'done)
; ---

; complex numbers
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ; 2.79
  (define (equ? x y)
    ((get 'equ? (list (type-tag x) (type-tag y))) (contents x) (contents y)))
  ; ---
  ; 2.80
  (define (=zero? a)
    ((get '=zero? (list (type-tag a))) (contents a)))
  ; ---
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ; 2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude-complex)
  (put 'angle '(complex) angle)
  ; ---
  ; 2.79
  (put 'equ? '(complex complex) equ?)
  ; ---
  ; 2.80
  (put '=zero? '(complex) =zero?)
  ; ---
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude-complex z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (square x) (* x x))
; ---

(display "2.81")(newline)

(display "Installing packages...")(newline)
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(display " --- ")(newline)

; Coercion

(define (put-coercion from to func)
  (put 'coercion (list from to) func))
(define (get-coercion from to)
  (get 'coercion (list from to)))

(define (scheme-number->scheme-number n) n)
(define (rational->rational r) r)
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'rational 'rational rational->rational)
(put-coercion 'complex 'complex complex->complex)

(display "test basic coercion input")(newline)
(put-coercion 't1 't2 (lambda (x y) (+ x y)))
((get-coercion 't1 't2) 1 2)
(newline)

; coercion procedures

; number -> rational -> (real ->) complex

(put-coercion
 'scheme-number
 'rational
 (lambda (n)
   ;(display "making rational: ")(display n)(newline)
   (make-rational (contents n) 1)))
(put-coercion
 'rational
 'complex
 (lambda (n)
   (make-complex-from-real-imag n 0)))

(make-rational 1 3)
((get-coercion 'scheme-number 'rational) (make-scheme-number 1))

(newline)
(display "add numbers of same type")(newline)
(add (make-scheme-number 5) (make-scheme-number 2))
(add (make-rational 1 2) (make-rational 1 4))
(add (make-complex-from-real-imag 2 5) (make-complex-from-real-imag 2 4))

(newline)
(display "convert types")(newline)
((get-coercion 'scheme-number 'rational) (make-scheme-number 2))
((get-coercion 'rational 'complex) (make-rational 1 2))

(newline)
(display "add numbers of different types")(newline)
(display "  scheme-number - rational")(newline)
(add (make-scheme-number 1) (make-rational 1 2))
(add (make-rational 1 2) (make-scheme-number 1))
(display "  rational complex")(newline)
(add (make-rational 1 2) (make-complex-from-mag-ang 2 3))