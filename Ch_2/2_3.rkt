#lang sicp

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

; Exercise 2.3:
; Implement a representation for rectangles in
; a plane. (Hint: You may want to make use of Exercise 2.2.) In
; terms of your constructors and selectors, create procedures
; that compute the perimeter and the area of a given rectangle.
; Now implement a different representation for rectangles.
; Can you design your system with suitable abstraction
; barriers, so that the same perimeter and area procedures
; will work using either representation?

; rectangle
; x, y, w, h
; p (point, position), s (point, size)

; this implementation only supports right-angles rectangles,
; not rotated rectangles
(define (rec4 x y w h) (cons (cons x y) (cons w h)))
(define (rec2 p s) (cons p s))



(define (perimeter r)
  )
(define (area r)
  )

; not finished, but "correct" solution uses different x/y/w/h functions,
; they are not as generic as originally understood