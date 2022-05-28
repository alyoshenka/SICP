#lang racket

; Define an encoding tree and a sample message.
; Use the decode procedure to decode the message,
; and give the result.

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)
(define decoded-message (decode sample-message sample-tree))

; 2.68
(display "2.68")(newline)

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; returns the list of bits that encodes a given symbol
; according to a given tree
; design encode-symbol so that it signals an error if
; the symbol is not in the tree at all
(define (encode-symbol symbol tree)
  (cond ((null? tree) '())
        ((leaf? tree) '())
        ((member symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((member symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol: ENCODE-SYMBOL" symbol))))

(display "tree: ")(newline)
(display sample-tree)(newline)

(newline)
(display "encoded message: ")(display sample-message)(newline)
(display "decoded message: ")(display decoded-message)(newline)

(display sample-message)(display " = ")
(display (encode (decode sample-message sample-tree) sample-tree))
(newline)
(display decoded-message)(display " = ")
(display (decode (encode decoded-message sample-tree) sample-tree))
(newline)

; 2.69
(newline)(display "2.69")(newline)(newline)

; takes as its argument a list of symbol-frequency pairs (where
; no symbol appears in more than one pair) and generates a Huffman
; encoding tree according to the Huffman algorithm
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if ((null? (cdr leaf-set)) leaf-set)
      
    ; 1 elem: return
    ; else merge 2 smallest
    ; ordered set: merge first 2 (increasing weights)