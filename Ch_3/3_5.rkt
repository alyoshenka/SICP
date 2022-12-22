#lang racket

#|
Implement Monte Carlo integration as a procedure estimate-
integral that takes as arguments a predicate P, upper and
lower bounds x1, x2, y1, and y2 for the rectangle, and the
number of trials to perform in order to produce the esti-
mate. Your procedure should use the same monte-carlo
procedure that was used above to estimate π . Use your estimate-
integral to produce an estimate of π by measuring the
area of a unit circle.
|#

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (* 1.0 (/ trials-passed trials)))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low-inc high-exc)
  (let ((range (- high-exc low-inc)))
    (+ low-inc (* (random) range))))

; p(x,y) -> true for x,y in region
; region: (x-x1)^2 + (y-y1)^2 = r^2 -> circle
(define (p r x y)
  (lambda (x1 y1)
    (>=
     (* r r)
     (+
      (* (- x1 x) (- x1 x))
      (* (- y1 y) (- y1 y))))))

(define unit-circle (p 1 0 0))
  
; A = pi * r^2 -> unit circle r = 1, so A = pi
; fraction * area = estimate
(define (estimate-integral predicate x-low x-high y-low y-high)
  (define (area) (* (- x-high x-low) (- y-high y-low)))
  (define (experiment)
    (let ((x1 (random-in-range x-low x-high))
          (y1 (random-in-range y-low y-high)))
      ;(display "x1: ")(display x1)(display " y1: ")(display y1)(newline)
      ;(display "pred (unit)?: ")(display (predicate x1 y1))(newline)
      (predicate x1 y1)))      
  experiment)
      
(display "random numbers")(newline)
(random-in-range 1 2) ; 1
(random-in-range 1 11)
(random-in-range 1 11)
(random-in-range 1 11)

(display "predicate")(newline)
(define test-circle (p 3 5 7))
(test-circle 5 7) ; #t
(test-circle 2 7)
(test-circle 5 10)
(test-circle 8 7)
(test-circle 5 4)
(test-circle 0 0) ; #f


(display "unit circle")(newline)
(unit-circle 0 1) ; #t
(unit-circle 1 0)
(unit-circle -1 0)
(unit-circle 0 -1)
(unit-circle 0 0)
(unit-circle 1 1) ; #f

(newline)
(display "simulations")(newline)
(display "10:")(newline)
(monte-carlo 10 (estimate-integral unit-circle -1.0 1.0 -1.0 1.0))
(display "100:")(newline)
(monte-carlo 100 (estimate-integral unit-circle 0 1 0 1))
(display "1000:")(newline)
(monte-carlo 1000 (estimate-integral unit-circle 0 1 0 1))