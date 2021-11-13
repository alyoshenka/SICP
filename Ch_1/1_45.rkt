#lang sicp

; f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

; x^2
(define (square x) (* x x))
; x + 1
(define (inc x) (+ x 1))
; (x + y) / 2
(define (average x y)
  (/ (+ x y) 2))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    ;(display "trying: ")(display guess)(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; fast version
(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt b (/ n 2))))
        (else (* b (expt b (- n 1))))))


(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (4-root x)
  (fixed-point
   (repeated
    (average-damp
     (lambda (y) (/ x (expt y 3))))
    2)
   1.0))

(define (5-root x)
  (fixed-point
   ((repeated average-damp 2)
   (lambda (y) (/ x (expt y 4))))
   1.0))

(define (6-root x)
  (fixed-point
   ((repeated average-damp 2)
   (lambda (y) (/ x (expt y 5))))
   1.0))

(define (7-root x)
  (fixed-point
   ((repeated average-damp 2)
   (lambda (y) (/ x (expt y 6))))
   1.0))

; result
(define (n-root b e d)
  (fixed-point
   ((repeated average-damp d)
   (lambda (y) (/ b (expt y (- e 1)))))
   1.0))

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (log n 2))
   (lambda (y) (/ x (expt y (- n 1)))))
   1.0))


; tests
(n-root (expt 6 5) 5 2)
(n-root (expt 6 6) 6 2)
(n-root (expt 6 7) 7 2)
(n-root (expt 6 8) 8 3)
(n-root (expt 6 9) 9 3)


(display "---")