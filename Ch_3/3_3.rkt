#lang racket

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))
((acc 'withdraw) 50) ; 50
((acc 'withdraw) 60) ; "Insufficient funds"
((acc 'deposit) 40) ; 90
((acc 'withdraw) 60) ; 30

(display "Password protected:")(newline)

(define (make-account-pp balance password)
  (let ((pw password))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch p m)
      (cond ((not (eq? p pw)) (lambda (x) "Incorrect password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

(define pw-acc (make-account-pp 100 'secret-password))
((pw-acc 'secret-password 'withdraw) 40) ; 60
((pw-acc 'some-other-password 'deposit) 50) ; "Incorrect password"