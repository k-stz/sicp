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
