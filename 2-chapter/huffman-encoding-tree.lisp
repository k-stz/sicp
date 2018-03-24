(in-package :sicp)


;; prefix code: where the complete code for any symbol is not part of the beginning of the
;; code of another symbol. This solves the issue of variable length code, where when parsing
;; it we would have the problem of not knowing whether we've parsed a symbol, or are just
;; n-bits into parsing another symbol that whose prefix bits are the same as the whole
;; code of another single symbol

;; Why not use fixed size codes? This way we read a fixed amount of bits and know that we
;; have a unique symbol at hand. The advantage of variable length code is that it is
;; significantly more space efficient. (smaller storage size, faster transfer etc.)

;; How to construe variable size code? The best approach is to give symbols that are
;; used most frequently the smallest size. Just like in Morse-Code, the letter 'E' is
;; just a single dot.

;; Huffmann encoding tree,
;; At its nodes it holds a particular symbol and its relative frequency (tree height =
;; higher frequency).  Traversing the tree to a given symbol also generates the code of
;; the symbol. Such that moving left adds a 0 bit and right a 1 bit. Corollary letters
;; higher in the tree, that are reached sooner, have less overall encoding bits, which
;; matches nicely to them having a higher frequency!
;; By traversing the tree we follow a linear path to a symbol, and thus whenver we
;; reach a leaf, we can't ambiguously be in the prefix of another symbol, so this
;; also generates prefix-code!

;; Generating Huffmann Trees,
;; given this marvelous trick Davin Huffman braught to computer science, how can we generate trees
;; for any given "alphabet" of sybols?

;; David Huffman provided an algorithm and also showed that it also generates optimal
;; variable length prefix-code, with regards to length and relative frequency of the
;; symbols of a given alphabet


;; TODO implement huffman tree generating algorithm given

;; ((<symbol> <frequency>) ...)
(defparameter *initial-leaves* '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))


;; Functions operating the data structures

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf? (object)
  (eq (car object) 'leaf))

(defun symbol-leaf (x) (cadr x))

(defun weight-leaf (x) (caddr x))

(defun left-branch (tree) (car tree))

(defun right-branch (tree) (cadr tree))

(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(defun make-code-tree (left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(t 
	 (error "bad bit -- CHOOSE-BRANCH ~a" bit))))

(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
	     (if (null bits)
		 '()
		 (let ((next-branch (choose-branch (car bits) current-branch)))
		   (if (leaf? next-branch)
		       (cons (symbol-leaf next-branch)
			     (decode-1 (cdr bits) tree))
		       (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

;; adjoin-set will put new elements in the list in order of their `weight'

(defun adjoin-set (x set)
  (cond ((null set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(t (cons (car set)
		 (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)  ;symbol
			       (cadr pair));frequency
		    (make-leaf-set (cdr pairs))))))

;; exercise 2.67
(defparameter *sample-tree*
  (make-code-tree (make-leaf 'a 4)
		  (make-code-tree
		   (make-leaf 'b 2)
		   (make-code-tree (make-leaf 'd 1)
				   (make-leaf 'c 1)))))

(defparameter *simple-message* '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; (decode *simple-message* *sample-tree*) ==> (A D A B B C A)

;; exercise 2.68

(defun encode-symbol (symbol tree)
  (labels ((rec (tree code)
	     (let ((left-branch (left-branch tree))
		   (right-branch (right-branch tree)))
	       (if (leaf? tree)
		   code
		   (if (element-of-set? symbol (symbols left-branch))
		       (rec left-branch (cons 0 code))
		       (rec right-branch (cons 1 code)))))))
    ;; testing if symbol is even part of huffman-tree
    (if (element-of-set? symbol (symbols tree))
	(reverse (rec tree '()))
	(error "The symbol ~a is not in the tree!" symbol))))

(defun encode (message tree)
  (if (null message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))
