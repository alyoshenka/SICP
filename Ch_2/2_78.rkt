#lang racket

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond
    ((number? datum)
     'number)
    ((pair? datum)
     (car datum))
    (else
     (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond
    ((number? datum)
     datum)
    ((pair? datum)
     (cdr datum))
    (else
     (error "Bad tagged datum: CONTENTS" datum))))

(define p (cons 'typed 5))
(define n 5)

(attach-tag 'thingy n)
(attach-tag 'another-tag p)

(type-tag n)
(type-tag p)

(contents n)
(contents p)


