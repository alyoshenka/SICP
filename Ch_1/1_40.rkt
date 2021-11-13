#lang sicp


(define (square x) (* x x))
(define (cube x) (* x x x))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; ---------
; zeros -> f(x) = x

(define (cubic a b c) (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define x1 (newtons-method (cubic 2 3 4) 1))
(display "x1: ")(display x1)(newline)
(/ (round (* 100 ((cubic 2 3 4) x1))) 100)

(define x2 (newtons-method (cubic 5 4 7) 1))
(display "x2: ")(display x2)(newline)
(/ (round (* 100 ((cubic 5 4 7) x2))) 100)
  