#lang racket

(display "Password multiple access protected:")(newline)

(define (make-account-pp balance password)
  (let ((pw password) (attempts 0))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (set! attempts (if (eq? p pw) 0 (+ attempts 1)))
    (cond ((> attempts 7) (lambda (x) "Calling the cops"))
          ((not (eq? p pw)) (lambda (x) attempts))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch))

(define pw-acc (make-account-pp 100 'secret-password))
((pw-acc 'secret-password 'withdraw) 40) ; 60
((pw-acc 'some-other-password 'deposit) 50) ; "Incorrect password"

((pw-acc 'secret-password 'withdraw) 0)


; doesn't work as intended
(define (try f n)
  (display "try ")(display n)(display ": ")
  (cond ((> n 0)
         (display f)(newline)
         (try f (- n 1)))
        (else 'done)))
      

(sqrt 4)
(display "Compute sqrt 4, 3 times")(newline)
(try (sqrt 4) 3)


(display "Should not call the cops")(newline)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
(newline)
((pw-acc 'secret-password 'withdraw) 0)
(display "Should not call the cops")(newline)
((pw-acc 'bad 'withdraw) 0)

(newline)
((pw-acc 'secret-password 'withdraw) 0)
(display "Should call the cops")(newline)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)

(newline)
((pw-acc 'secret-password 'withdraw) 0)
(display "Should call the cops")(newline)

((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)

((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)
((pw-acc 'bad 'withdraw) 0)