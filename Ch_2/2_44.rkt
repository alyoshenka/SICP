#lang sicp

(#%require sicp-pict)

;(#%require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

#|
(#%require graphics/graphics)
(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))

(define (vector-to-posn v)
  (make-posn (ycor-vect v)
             (xcor-vect v)))
|#

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define wave4 (flipped-pairs einstein))



; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(display "demo")(newline)
(paint wave4)
(paint einstein)
(paint (right-split einstein 2))
(paint (corner-split einstein 2))
(paint (square-limit einstein 2))
(display "2.44: up-split")(newline)
(paint (up-split einstein 2))

(newline)
(display "flipped-pairs")(newline)
(paint (flipped-pairs einstein))
(display "square-limit")(newline)
(paint (square-limit einstein 0))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs-1-1 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define flipped-pairs-1-2
  (square-of-four identity flip-vert identity flip-vert))

#|
(define (square-limit-1-1 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
|#

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square-limit-1-2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  (compose flip-vert flip-horiz) flip-vert)))
    (combine4 (corner-split painter n))))

(display "flipped-pairs-1-1")(newline)
(paint (flipped-pairs-1-1 einstein))
(display "flipped-pairs-1-2")(newline)
(paint (flipped-pairs-1-2 einstein))
(display "square-limit-1-2")(newline)
(paint (square-limit-1-2 einstein 0))

; 2.45

#|
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
|#

(define (split op1 op2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))       
          (op1 painter (op2 smaller smaller)))))
  (lambda (painter n)
    (iter painter n)))

(define right-split-2 (split beside below))
(define up-split-2 (split below beside))
  

(newline)
(display "right-split-2")(newline)
(paint (right-split einstein 2))
(paint (right-split-2 einstein 2))
(display "up-split-2")(newline)
(paint (up-split einstein 2))
(paint (up-split-2 einstein 2))

; 2.46

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(newline)
(display "5 = ")(xcor-vect (make-vect 5 6))
(display "6 = ")(ycor-vect (make-vect 5 6))
(newline)

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))


#|
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (cdr (cdr frame)))
|#
(define (make-frame origin edge1 edge2)
   (list origin edge1 edge2))
(define (origin-frame frame)
   (car frame))
(define (edge1-frame frame)
   (cadr frame))
(define (edge2-frame frame)
   (caddr frame))

(display "frames")(newline)
(make-frame (make-vect 1 1) (make-vect 2 1) (make-vect 1 4))
(newline)


(define a-frame (make-frame
                 (make-vect 0 0)
                 (make-vect 1 0)
                 (make-vect 0 1)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(display "=")(newline)
((frame-coord-map a-frame) (make-vect 0 0))
(origin-frame a-frame)

(newline)

; 2.47
; already implemented
; get list vars by getting by index (1 2 3)

; 2.48
(define (make-segment v1 v2)
   (cons v1 v2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
   (cdr segment))

(display "segments")(newline)
(make-segment (make-vect 1 2) (make-vect 3 4))
(display "(1 2) = ")(start-segment (make-segment (make-vect 1 2) (make-vect 3 4)))
(display "(3 4) = ")(end-segment (make-segment (make-vect 1 2) (make-vect 3 4)))

; 2.49

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))




(define outline
  (segments->painter
   (list
    ; left
    (make-segment
     (make-vect 0.01 0.01)
     (make-vect 0.01 0.99))
    ; bottom
    (make-segment
     (make-vect 0.01 0.01)
     (make-vect 0.99 0.01))
    ; right
    (make-segment
     (make-vect 0.01 0.99)
     (make-vect 0.99 0.99))
    ; top
    (make-segment
     (make-vect 0.01 0.99)
     (make-vect 0.99 0.99)))))

(define corners
  (segments->painter
   (list
    (make-segment
     (make-vect 0.01 0.01)
     (make-vect 0.99 0.99))
    (make-segment
     (make-vect 0.01 0.99)
     (make-vect 0.99 0.01)))))

(define diamond
  (segments->painter
   (list
    ; top left
    (make-segment
     (make-vect 0 0.5)
     (make-vect 0.5 1))
    ; top right
    (make-segment
     (make-vect 0.5 1)
     (make-vect 1 0.5))
    ; bottom right
    (make-segment
     (make-vect 1 0.5)
     (make-vect 0.5 0))
    ; bottom left
    (make-segment
     (make-vect 0.5 0)
     (make-vect 0 0.5)))))

(define wave
  (segments->painter
    (list
     (make-segment
      (make-vect 0.165 0.945)
      (make-vect 0.465 0.665))
     (make-segment
      (make-vect 0.465 0.665)
      (make-vect 0.465 0.285)) 
     (make-segment
      (make-vect 0.465 0.455)
      (make-vect 0.745 0.585)) 
     (make-segment
      (make-vect 0.465 0.665)
      (make-vect 0.755 0.925)) 
     (make-segment
      (make-vect 0.475 0.455)
      (make-vect 0.185 0.615)) 
     (make-segment
      (make-vect 0.245 0.265)
      (make-vect 0.685 0.295))
     (make-segment
      (make-vect 0.685 0.295)
      (make-vect 0.685 0.035)) 
     (make-segment
      (make-vect 0.685 0.035)
      (make-vect 0.245 0.065)) 
     (make-segment
      (make-vect 0.245 0.065)
      (make-vect 0.245 0.265)))))

(define window-frame
  (make-frame
   (make-vect 0 0)
   (make-vect 500 0)
   (make-vect 0 500)))

(define top-left
  (make-frame
   (make-vect 0 0)
   (make-vect 250 0)
   (make-vect 0 250)))
(define top-right
  (make-frame
   (make-vect 250 250)
   (make-vect 250 0)
   (make-vect 0 250)))
(define bottom-left
  (make-frame
   (make-vect 0 0)
   (make-vect 250 0)
   (make-vect 0 250)))
(define bottom-right
  (make-frame
   (make-vect 250 0)
   (make-vect 500 0)
   (make-vect 250 250)))
  
(newline)
(display "simple painters")(newline)
;(outline window-frame)
;(outline top-left)
;(outline top-right)
;(diamond bottom-left)
;(diamond bottom-right)

