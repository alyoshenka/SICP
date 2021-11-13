#lang sicp

; rp = 1/(1/r1 + 1/r2)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

#|
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
|#

#|
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))
|#

(define (make-interval a b) (cons a b))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


; 2.7
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

; 2.8
; the new interval minimum should be the difference
; of the two minimums, likewise for the new maximum
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; 2.9
; Adding/subtracting intervals only "shifts" the width
; up or down. Multiplying/dividing intervals skews the
; width.
; This is similar to significant figures in physics/science:
; A/S operations preserve the number of places after the
; decimal point, while M/D operations preserve the number
; of significant figures.

; i1: (3, 5), w1 = 1
; i2: (2, 8), w2 = 3
; (i1 + i2) w = 4 = w1 + w2
; (i1 * i2) w = 17.5

; 2.10
(define (div-interval x y)
  (if (and (>= 0 (lower-bound y)) (<= 0 (upper-bound y)))
      (error "attempt to divide by 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

; 2.11
(define (mul-interval x y)
  ; F: first
  ; S: second
  ; L: lower
  ; U: upper
  ; +: positive
  ; -: negative
  (let ((FL (lower-bound x))
        (FU (upper-bound x))
        (SL (lower-bound y))
        (SU (upper-bound y)))
    (let ((+FL (<= 0 FL))
          (+FU (<= 0 FU))
          (+SL (<= 0 SL))
          (+SU (<= 0 SU)))
      (let ((-FL (not +FL))
            (-FU (not +FU))
            (-SL (not +SL))
            (-SU (not +SU)))
  (cond
    ((and +FL +FU +SL +SU)
     (make-interval (* FL SL) (* FU SU)))
    ((and +FL +FU -SL +SU)
     (make-interval (* FU SL) (* FU SU)))
    ((and +FL +FU -SL -SU)
     (make-interval (* FU SL) (* FL SU)))
    ((and -FL +FU +SL +SU)
     (make-interval (* FL SU) (* FU SU)))
    ((and -FL +FU -SL +SU) ; check case
     (make-interval (min (* FL SU) (* FU SL)) (* FU SU)))
    ((and -FL +FU -SL -SU)
     (make-interval (* FU SL) (* FL SL)))
    ((and -FL -FU +SL +SU)
     (make-interval (* FL SU) (* FU SL)))
    ((and -FL -FU -SL +SU)
     (make-interval (* FL SU) (* FL SL)))
    ((and -FL -FU -SL -SU)
     (make-interval (* FU FU) (* FL SL))))))))

; 2.12

; ratio of width of interval to midpoint of interval
; ex: i = (5, 15) m = 10 p = 50

; percent 0-1
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent i)
  (* 100 (/ (width i) (center i))))

; 2.13
; https://billthelizard.blogspot.com/2010/12/sicp-212-216-extended-exercise-interval.html
; p = pa + pb

; 2.14
; not finished
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))
(define (disp-intr-cp i)
  (display " c=")(display (center i))
  (display ", p=")(display (percent i))(display "%\n"))

(define a (make-center-percent 57 1.5))
(define b (make-center-percent 48 1.1))
(display "a: ")(disp-intr-cp a)
(display "b: ")(disp-intr-cp b)
(display "p1: ")(disp-intr-cp (par1 a b))
(display "p2: ")(disp-intr-cp (par2 a b))
(display "a/a: ")(disp-intr-cp (div-interval a a))
(display "a/b: ")(disp-intr-cp (div-interval a b))

; 2.15
; Yes, when uncertainty repeats, it produces a larger uncertainty
; 2.16
; no clue

; tests

#|

(newline)
(display "center, percent")(newline)
(define i (make-center-percent 10 0.5)) 
(= 5 (lower-bound i))
(= 15 (upper-bound i))
(= 10(center i)) 
(= 0.5 (percent i)) 

(define (=i a b)
  (and (=
        (lower-bound a)
        (lower-bound b))
       (=
        (upper-bound a)
        (upper-bound b))))

(define a (make-interval 2 4))
(define b (make-interval -2 4))
(define c (make-interval -4 -2))

(define s (make-interval (- 10) 10))

(newline)
(display "div-interval")(newline)
; should produce error
; (div-interval a s))

(newline)
(display "mul-interval")(newline)
(display "++ ++: ")
(=i (mul-interval a a) (make-interval 4 16))
(display "++ -+: ")
(=i (mul-interval a b) (make-interval (- 8) 16))
(display "++ --: ")
(=i (mul-interval a c) (make-interval (- 16) (- 4)))
(display "-+ ++: ")
(=i (mul-interval b a) (make-interval (- 8) 16))
(display "-+ -+: ")
(=i (mul-interval b b) (make-interval (- 8) 16))
(display "-+ --: ")
(=i (mul-interval b c) (make-interval (- 16) 8))
(display "-- ++: ")
(=i (mul-interval c a) (make-interval (- 16) (- 4)))
(display "-- -+: ")
(=i (mul-interval c b) (make-interval (- 16) 8))
(display "-- --: ")
(=i (mul-interval c c) (make-interval 4 16))

(define i1 (make-interval 5 15))
(define i2 (make-center-width 10 5))
(define i3 (make-center-percent 10 50))

(newline)
(display "percent")(newline)
; should all return true (#t)
(= (lower-bound i1) (lower-bound i2) (lower-bound i3))
(= (upper-bound i1) (upper-bound i2) (upper-bound i3))
(= (center i1) (center i2) (center i3))
(= (percent i1) (percent i2) (percent i3))
(newline)
(define a (make-center-percent 5 .02))
(define b (make-center-percent 10 .03))
(define c (mul-interval a b))
(let ((p (* 100 (percent c))))
  (and (< 4.98 p) (> 5 p)))

|#