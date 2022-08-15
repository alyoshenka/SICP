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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

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

#|
(define (successive-merge leaf-set)
  (define (iter leaf-set node)
    (if (null? (cdr leaf-set)) leaf-set
        (let ((s1
               (if (leaf? (car leaf-set))
                   (list (symbol-leaf (car leaf-set)))
                   (car (car leaf-set))))
              (w1
               (if (leaf? (car leaf-set))
                   (weight-leaf (car leaf-set))
                   (cadar leaf-set)))
              (s2
               (if (leaf? (cadr leaf-set))
                   (list (symbol-leaf (cadr leaf-set)))
                   (caadr leaf-set)))
              (w2
               (if (leaf? (cadr leaf-set))
                   (weight-leaf (cadr leaf-set))
                   (cdadr leaf-set))))
          (let ((new-pair (list (append s1 s2) (+ w1 w2))))
            (iter (cons new-pair (cddr leaf-set)))))))
  (car (iter leaf-set '())))
|#

(define (successive-merge leaf-set)
  (cond ((null? set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        (else (successive-merge
               (adjoin-set (make-code-tree
                            (car leaf-set)
                            (cadr leaf-set))
                           (cddr leaf-set))))))
  

(define leafs (list (list 'D 1)(list 'C 1)(list 'B 2)(list 'A 4)))
;(successive-merge leafs)
(generate-huffman-tree leafs)
      
; 2.70
(newline)(display "2.70")(newline)(newline)

(define rock-pairs (list (list 'WAH 1)(list 'BOOM 1)
                         (list 'A 2)(list 'GET 2)(list 'JOB 2)
                         (list 'SHA 3)
                         (list 'YIP 9)
                         (list 'NA 16)))
;(display "rock pairs: ")(display rock-pairs)(newline)
;(display "rock leafs: ")(display (make-leaf-set rock-pairs))(newline)

(define rock-tree (generate-huffman-tree rock-pairs))
(display "rock tree: ")(display rock-tree)(newline)

(newline)
(display "message")(newline)
(define song (list
              'GET 'A 'JOB
              'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
              'GET 'A 'JOB
              'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
              'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP
              'SHA 'BOOM))
(encode song rock-tree)

; 84 bits to encode ^
; 108 with fixed length

(+ 16 (* 2 9)(* 4 2) 5 5 (* 4 3) (* 5 2) (* 5 2))
(* 3 (+ 16 9 3 2 2 2 1 1))

; 2.71

; 1 for the most
; n-1 for the least

; 2.72
#|
rf: 1,2,4,2^(n-1)

order of growth to encode most frequent symbol (1 bits)

O(1)
constant, because it checks the left tree first,
and there is only one symbol in the left tree: the
one we're looking for. returns on next iteration

order of growth to encode least frequent symbol (n-1 bits)

n (*1.5) because at every level it must search n-l symbols to find the last one, where l is the level in the tree

|#