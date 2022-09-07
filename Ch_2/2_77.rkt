#lang racket

(require "./operations_table.rkt")
(require "./generic_arithmetic_package.rkt")

(display "2.77")(newline)

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)


(make-scheme-number 2)
(make-rational 4 8)
(make-complex-from-real-imag 3 4)

(define z (make-complex-from-real-imag 3 4))
(magnitude z)
