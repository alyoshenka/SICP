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
  (display "op: ")(display op)(newline)
  (display "args: ")(display args)(newline)
  (define (attempt-coercion args)
    (display "attempting to coerce: ")(display args)(newline)
    (if (pair? args)
        0
        args))
    
  (let ((args (car args)))  
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))          
            (cond
              ((= (length args) 2)
               (let ((type1 (car type-tags))
                     (type2 (cadr type-tags))
                     (a1 (car args))
                     (a2 (cadr args)))
                 (let ((t1->t2 (get-coercion type1 type2))
                       (t2->t1 (get-coercion type2 type1)))
                   (cond
                     ((eq? type1 type2)
                      ((error "No method for these types"
                              (list op type-tags))))
                     (t1->t2
                      (apply-generic op (list (t1->t2 a1) a2)))
                     (t2->t1
                      (apply-generic op (list a1 (t2->t1 a2))))
                     (else (error "No method for these types"
                                  (list op type-tags)))))))
              ; attempt to do it one-by one
              ((< (length args) 2)
               (error "No method for these types" (list op type-tags)))
              (else
               ; apply-generic to the first 2,
               ; cons to list,
               ; apply-generic to remaining list
               (let ((first (car args))
                     (second (cadr args))
                     (remaining (caddr args)))
                 (let ((res (apply-generic op (list first second))))
                   (let ((new-args (list res remaining)))
                     (apply-generic op new-args)))))))))))
               

; generic arithmetic procedures
; (define (add x y) (apply-generic 'add x y))
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
  ; 2.81
  ;; following added to Scheme-number package
  (put 'exp '(scheme-number scheme-number)
       (lambda (x y) (tag (expt x y))))
  ; using primitive expt
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
; ---

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

#|
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
(display "  rational - complex")(newline)
;(add (make-rational 1 2) (make-complex-from-mag-ang 2 3))
; this won't work because complex doesn't have a way to add rational numbers
; in its constructor.
|#

#|
Exercise 2.82: Show how to generalize apply-generic to
handle coercion in the general case of multiple arguments.
One strategy is to aempt to coerce all the arguments to
the type of the first argument, then to the type of the sec-
ond argument, and so on. Give an example of a situation
where this strategy (and likewise the two-argument ver-
sion given above) is not sufficiently general. (Hint: Con-
sider the case where there are some suitable mixed-type
operations present in the table that will not be tried.)

if args length is one, the fact that no procedure has been
found means there is not one for the single type. this should
result in an error
else, essentially coerce all arguments to the "highest" type
coercing the first to second type, first two to third type, etc
will fail if the first argument is the highest in the type tower

I guess you could coerce forward then backward, to ensure that
you are finding the highest type

scheme-number, rational, complex, complex, rational
-> rational, rational, complex, complex, rational
   complex, complex, complex, complex, rational
   complex, complex, complex, complex, rational*

maybe it in fact does work

first to second
first and second to third
...

keep a record to the highest type encountered
ie if you *can* coerce a type

rational, scheme-number, complex

can you get this done with one pass of the list? No, I don't think so

highest: rational
scheme-number -> rational
highest: complex
rational (both) -> complex

**Any time you encounter a higher type than before, coerce all items
to that type**

This will not coerce more than necessary (if only single step coercion
procedure are defined) because all coercion steps would need to be
applied anyway. However, it does involve multiple passes over the list
for any time a "higher" type is encountered.

But there still needs to be a heirarchy of types
What happens if there is a 2+ "floor" jump of types?
scheme-number, complex
there is no scheme-number->complex coercion, so how do we know that
it can be coerced? Do we have to put every combination into the table?
That seems inefficient.

|#

(newline)
(display "2.82")(newline)
(newline)

(display "redefining 'add' to allow for variable number of arguments")(newline)
(define (add . args) (apply-generic 'add args))

(put-coercion
 'scheme-number
 'complex
 (lambda (n)
   (make-complex-from-real-imag ((get-coercion 'scheme-number 'rational) n) 0)))

(display "test multi-step coercion")(newline)
((get-coercion 'scheme-number 'complex) (make-scheme-number 5))

(newline)
(display "test 0 and 1 argument add")(newline)
;(add)
;(add (make-scheme-number 1))


(newline)
(display "test add with 2 arguments, of the same type")(newline)
(add (make-scheme-number 1) (make-scheme-number 2))

(newline)
(display "test add with > 2 arguments, of the same type")(newline)
(add
  (make-scheme-number 1)
  (make-scheme-number 2)
  (make-scheme-number 3)
  (make-scheme-number 4))

(newline)
(display "test add with 2 arguments, of different types")
(newline)
(add (make-scheme-number 1) (make-rational 1 2))
(add (make-rational 1 2) (make-scheme-number 1))

(newline)
(display "test add with > 2 arguments, of different types, with increasing 'height'")
(newline)
(add
 (make-scheme-number 1)
 (make-rational 2 3)
 (make-complex-from-real-imag 4 5))

; getting convoluted, moving on in the interest of progress