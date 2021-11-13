#lang sicp

; (define same-parity (lambda (orig . x) <body>))
(define (same-parity orig . x)
  (let
      ((idx 0)
       (len (length x)))
    
    (define (add-elem idx)
      (cond
        ((= len idx)
         nil)
        ((= (remainder (list-ref x idx) 2) (remainder orig 2))
         (cons (list-ref x idx) (add-elem (+ idx 1))))
        (else
         (add-elem (+ idx 1)))))
          
    
    (cons orig (add-elem 0))))
          
  

; 1 3 5 7
(same-parity 1 2 3 4 5 6 7)
; 2 4 6
(same-parity 2 3 4 5 6 7)