#lang sicp
(#%require sicp-pict)
 

(define (add-vect v1 v2)
  (make-vect (+ (vector-xcor v1)
                (vector-xcor v2))
             (+ (vector-ycor v1)
                (vector-ycor v2))))
 
(define (sub-vect v1 v2)
  (make-vect (- (vector-xcor v1)
                (vector-xcor v2))
             (- (vector-ycor v1)
                (vector-ycor v2))))
 
(define (scale-vect n v)
  (make-vect (* (vector-xcor v) n)
             (* (vector-ycor v) n)))

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define first (lambda (l) (list-ref l 0)))
(define second (lambda (l) (list-ref l 1)))
(define third (lambda (l) (list-ref l 2)))

(first (list 1 2 3))
(second (list 1 2 3))
(third (list 1 2 3))
 
(define frame1-origin first)
(define frame1-edge1  second)
(define frame1-edge2  third)

(define (make-segment v1 v2)
  (list v1 v2))
 
(define start-segment first)
(define end-segment second)

(define frame-outline-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 1 1) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 0 0)))))
 
(define x-painter
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))
 
(define diamond-painter
  (segments->painter
   (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))
 
(define wave-painter-segments
  (list (make-segment (make-vect 0.2 0.0) (make-vect 0.4 0.4))
        (make-segment (make-vect 0.4 0.4) (make-vect 0.3 0.5))
        (make-segment (make-vect 0.3 0.5) (make-vect 0.1 0.3))
        (make-segment (make-vect 0.1 0.3) (make-vect 0.0 0.6))
        (make-segment (make-vect 0.0 0.8) (make-vect 0.1 0.5))
        (make-segment (make-vect 0.1 0.5) (make-vect 0.3 0.6))
        (make-segment (make-vect 0.3 0.6) (make-vect 0.4 0.6))
        (make-segment (make-vect 0.4 0.6) (make-vect 0.3 0.8))
        (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1.0))
        (make-segment (make-vect 0.6 1.0) (make-vect 0.7 0.8))
        (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.6))
        (make-segment (make-vect 0.6 0.6) (make-vect 0.8 0.6))
        (make-segment (make-vect 0.8 0.6) (make-vect 1.0 0.4))
        (make-segment (make-vect 1.0 0.2) (make-vect 0.6 0.4))
        (make-segment (make-vect 0.6 0.4) (make-vect 0.8 0.0))
        (make-segment (make-vect 0.7 0.0) (make-vect 0.5 0.3))
        (make-segment (make-vect 0.5 0.3) (make-vect 0.3 0.0))))

(paint-hires frame-outline-painter)
 

; 2.50
(newline)
(display "2.50")(newline)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) ; new origin
                     (make-vect 1.0 1.0) ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(paint einstein)