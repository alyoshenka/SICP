#lang sicp

; Exercise 2.2:
; Consider the problem of representing line
; segments in a plane. Each segment is represented as a pair
; of points: a starting point and an ending point. Define a
; constructor make-segment and selectors start-segment and
; end-segment that define the representation of segments in
; terms of points. Furthermore, a point can be represented
; as a pair of numbers: the x coordinate and the y coordinate.
; Accordingly, specify a constructor make-point and
; selectors x-point and y-point that define this representation.
; Finally, using your selectors and constructors, define a
; procedure midpoint-segment that takes a line segment as
; argument and returns its midpoint (the point whose coordinates
; are the average of the coordinates of the endpoints).

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; line segment
; takes (i)nitial and (f)inal points
(define (make-segment i f) (cons i f))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s) (make-point (/ (+ (x-point (start-segment s))
                                               (x-point (end-segment s)))
                                            2)
                                         (/ (+ (y-point (start-segment s))
                                               (y-point (end-segment s)))
                                            2)))

; point
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

; test
(define a (make-point 0 0))
(define b (make-point 6 4))
(define s (make-segment a b))
(display "a: ")(print-point a)
(display "\nb: ")(print-point b)
(display "\nm: ")(print-point (midpoint-segment (make-segment a b)))



