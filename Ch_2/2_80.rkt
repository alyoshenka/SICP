#lang racket

(require "./generic_arithmetic_package.rkt")

(display "Installing packages...")(newline)
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(display " --- ")(newline)

(display "Making numbers...")(newline)
(make-rectangular 1 2)
(make-polar 1 2)
(make-scheme-number 1)
(make-rational 1 2)
(make-complex-from-real-imag 1 2)
(make-complex-from-mag-ang 1 2)
(display " --- ")(newline)

(display "checking for =zero?")(newline)
(=zero? (make-scheme-number 0))
(=zero? (make-rational 0 2))
(=zero? (make-complex-from-mag-ang 0 2))
(=zero? (make-complex-from-real-imag 0 0))