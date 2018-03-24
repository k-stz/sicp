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

;; exercise 2.69

(defparameter *pairs* '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)))

;; use `make-code-tree' to implement `successive-merge'
;; use `make-leaf-set' to transform a list of pairs into an ordered set of leaves

(defun successive-merge (leaf-set)
  (if (<= (length leaf-set) 1)
      (car leaf-set) ; end case
      (successive-merge
       (adjoin-set ; sort after merge
	;; now first two elements always have smallest weight
	(make-code-tree (first leaf-set)
			(second leaf-set))
	(cddr leaf-set)))))

(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

;; exercise 2.70

(defparameter  *rock-symbols* '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

;; (encode '(get a job
;; 	  sha na na na na na na na na
;; 	  get a job
;; 	  sha na na na na na na na na
;; 	  Wah yip yip yip yip yip yip yip yip yip
;; 	  sha boom)
;; 	(generate-huffman-tree *rock-symbols*))
==> (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1
   0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1
   0 1 1 0 1 1)
;; Bits needed (length *) ==> 84
;; What would be the smallest number of bits needed if we used fixed-length code to encode
;; the eight-symbols of the rock alphabet in *rock-symbols*?
;; To distinguish 8 different symbols we need (log 8 2) => 3 Bits.
;; The song has 36 Words so we'd need: (* 3 36) ==> 108 Bits!

;; exercise 2.71
;; Make a huffman of n symbols, with frequencies 1,2,3...2^n. How many bits are needed
;; to represent the least and most frequent symbol?
;; Answer: Drawing smaller example of n=5 or n=10 one can see that the huffman tree is
;; highly unbalanced. That's because the generation algorithm mandates that we always
;; join the least frequent one first, adding them into a node and continuing with their
;; weights summed.
;; For the first two we get 1 + 2 = 3, The next element is 4, and because 3 < 4, we
;; have to join those to 7, but the next is 8 so we have to continue like this.
;; We never only get to join the first two free leaves and from then on add to them
;; the element because the weight of the other elements is always bigger then the
;; sum of the two previous: 2^n > (2^(n-1) + 2^(n-2)).

;; Thus the _least frequent symbol_ is at the end of the unbalanced tree with a depth
;; of n-1, and thus needs n-1 Bits.

;; While the most frequent symbol is, being the last one merged with the tree is the
;; first left-branch of the root of the tree and thus needs only 1 bit.

;; exercise 2.72
;; What's the order of growth of the `encode' procedure?
;; If we consider the case of exercise 2.71, a huffman tree given n symbols can
;; have at most a depth of n-1.
;; Since encode on each node has to loop over the symbol list to know which
;; branch to take down to the symbol its looking for we need to ascend n-1
;; steps and look up n-1 symbols at each step. So (n-1)^2, (n²-2n+1) steps or in Big-O
;; notation: O(n²)
