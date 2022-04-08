#lang sicp

(car ''abracadabra)

; (quote (quote abracadabra))
; This is making a quote of the quote of the symbol
; quote denotes lists (or symbols), so by quoting
; (quote sym), quote becomes the car
; the quote returned is just a suymbol, not a procedure