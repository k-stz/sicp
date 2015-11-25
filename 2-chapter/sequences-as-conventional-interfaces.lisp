(defpackage :sicp (:use :cl))
(in-package :sicp)

;; /Conventional Interfaces/----------------------------------------------------

(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
	((atom tree)
	 (if (oddp tree)
	     (square tree)
	     0))
	(t ;;else
	 (+ (sum-odd-squares (car tree))
	    (sum-odd-squares (cdr tree))))))


;; /Sequence Operations/--------------------------------------------------------

;; (filter #'oddp (list 1 2 3 4)) ==> (1 3)
(defun filter (predicate sequence)
  "Return sequence of elements satisfying the predicate."
  (let ((head (car sequence))
	(rest (cdr sequence)))
    (cond ((null (car sequence)) nil)
	  ((funcall predicate head)
	   (cons head (filter predicate rest)))
	  (t ;; else
	   (filter predicate rest)))))

;; (accumulate #'+ 0 (list 1 2 3)) ==> 6
(defun accumulate (op initial sequence)
  "Accumulate elements of sequence using a binary operation (op)."
  (if (null sequence)
	initial
	(funcall op (car sequence)
		 (accumulate op initial (rest sequence)))))

(defun enumerate-interval (low high)
  "Return list of integers from <low> to <high>"
  (if (> low high)
      nil
      (cons low (enumerate-interval (1+ low)
				    high))))

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t ; else
	 (append (enumerate-tree (car tree))
		 (enumerate-tree (cdr tree))))))


;;; sequence operation examples

(defun 2-sum-odd-squares (tree)
  (accumulate #'+ 0 ;; sum
	      (map 'list #'square ;; squares
		   (filter #'oddp ;; oddly
			   (enumerate-tree tree)))))


;;;for exercise 1.9 quick fib-iter reimplementation:
(defun fib-iter (a b n)
  (if (= n 0)
      a
      (fib-iter b (+ a b) (1- n))))
;;state transformations:
; a <- b
; b <- a + b
(defun fib (n)
  (fib-iter 0 1 n))

(defun even-fibs (n)
  (accumulate #'+ 0
	      (filter #'evenp
		      (map 'list #'fib
			   (enumerate-interval 0 n)))))


;; Exercise 2.33

;; implement map in terms of accumulate
(defun accu-map (fn sequence)
  "Implementation of MAP using ACCUMULATE"
  (accumulate
   (lambda (x y)
     (cons (funcall fn x)
	   y))
   nil
   sequence))


(defun accu-append (seq1 seq2)
  "Implementation of APPEND using ACCUMULATE"
  (accumulate #'cons seq2 seq1))

(defun accu-length (sequence)
  "Implementation of LENGTH using ACCUMULATE"
  (accumulate
   (lambda (x y)
     (declare (ignore x))
     ;; the second argument is building the deferred chain of (+ 1 <here>)
     ;; which terminates in a "0" - the initial element. Hence we add
     ;; the branches containing a (+ 1 ..) effectively returning the
     ;; the length of the list
     (+ 1 y))
   0 sequence))


;; Exercise 2.34

(defun horner-eval (x coefficient-sequence)
  "Evaluate a polynomial in x at a given value in x. Example: f(x)= 1 + 3x + 5x^3 + x^5 at x=2 would be (horner-eval 2 (list 1 3 0 5 0 1).

This function implements Horner's Rule: It can be proven that any algorithm that computes
arbitrary polynomials must at least use as man additions and multiplications as Horner's
rule, thus Horner's Rule is an optimal algorithm for polynomial evaluation!

This was proved (for the number additions) by A.M. Otrowski in 1954 paper that essentially
founded the modern study of optional algorithms. While V.Y.Pan 1966 proved it for
multiplications."
  (accumulate (lambda (this-coeff higher-terms)
		;; it helps to imagine it "backwards" starting at the initial=0 then, as
		;; according to Horner's rule "we multiply by x" (* x 0) "then add a.n-1
		;; (* this-coeff (* x 0)) since we're at the end of the deferred chain
		;; "this-coeff" is indeed the last value in the coefficient-sequence. Now
		;; it should be possible to see that the rest of the chain will follow the
		;; order: (... (a.nx + a.n-1)x + ... +a1)x + a0
		(+ this-coeff
		   (* higher-terms x)))
	      0
	      coefficient-sequence ))


;; Exercise 2.35

(defun accu-count-leaves (tree)
  "Implementation of COUNT-LEAVES using ACCUMULATE"
  ;; add list of 1's
  (accumulate #'+ 0
	      (accu-map
	       ;; map 1 to each leaf
	       (lambda (x) (declare (ignorable x))
		       1)
			;; lay out leaves in a list
			(enumerate-tree tree))))

;; Exercise 2.36

(defun accumulate-n (op init seqs)
  "Accumulates the elements under the operation with the same position in the sequences 
into a new sequence."
  (if (null (car seqs))
      nil
      (cons
       ;; only operate on the FIRST of the sequences
       (accumulate op init (accu-map #'first seqs))
       ;; move down the sequences simultaneously
       (accumulate-n op init (accu-map #'cdr seqs)))))

;;(accumulate-n #'+ 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ==> (22 26 30)


;; Exercise 2.37

(defun dot-product (v w)
  (accumulate #'+ 0 (map 'list #'* v w)))


;; NOTE: a much succincter implementation is to use DOT-PRODUCT instead of the
;;       egg scrambling mnemonic.
(defun matrix-*-vector (m v)
  "Perform matrix-vector multiplication. Matrix m is a list of lists (rows!)  and v, the
vector, is also a list."
  (map 'list
       ;; unite them as a sequence 
       (lambda (row)
	 ;; "scramble the eggs" together
	 (accumulate #'+ 0
		     ;; break the "vector-eggs" on top of the matrix columns, imagine how
		     ;; the columns overflow - trickling down along them
		     (map 'list #'* row v))) m))


(defparameter *m* '((1 2 3) (4 5 6) (7 8 9)))

(defun transpose (mat)
  "Transpose matrix mat. mat is a list-of-lists row-major matrix."
  (accumulate-n #'cons '()
		mat))

(defun matrix-*-matrix (m n)
  "Multiply matrix. m and n are list-of-lists row-major matrices."
  (let ((n-cols (transpose n)))
    (map 'list #'(lambda (m-row)
		   (map 'list (lambda (n-col)
				(dot-product m-row n-col))
			 n-cols)) m)))

;; alternatively: though it twists my idea of matrix multiplication this
;;                solution reuses code. Due to my preconceptions of matrix
;;                multiplication it provides a fresh new way to look at it

(defun 1-matrix-*-matrix (m n)
  "Multiply matrix. m and n are list-of-lists row-major matrices. (alternative
implementation to MATRIX-*-MATRIX"
  (let ((cols (transpose n)))
    (map 'list (lambda (row)
		 (matrix-*-vector cols row))
	 m)))


;; Exercise 2.38

(defun fold-left (op initial sequence)
  "Opposite of FOLD-RIGHT. First applies operation on <initial> and the element at the end of
the sequence working backwards through it."
  (labels ((iter (result rest)
		 (if (null rest)
		     result
		     ;; the first argument is the crucial difference to the recursive
		     ;; implementation of ACCUMULATE. See how we build the deferred
		     ;; chain upward the list resolving it backwards?
		     (iter (funcall op result (first rest))
			   (rest rest)))))
    (iter initial sequence)))

(defun fold-right (op initial sequence)
  "ACCUMULATE is also known as FOLD-RIGHT, because it combines the first
element of the sequence with the result of combining all the elements to the right."
  (accumulate op initial sequence))

;; division is a noncommutative operation:
;; (fold-right #'/ 1 (list 1 2 3)) => 3/2
;; (fold-left  #'/ 1 (list 1 2 3))  => 1/6
;; and so is LIST so to speak
;; (fold-right #'list nil (list 1 2 3)) => (1 (2 (3 NIL)))
;; (fold-left  #'list nil (list 1 2 3)) => (((NIL 1) 2) 3)

;; An operation must be _commutative_ in order for FOLD-LEFT and FOLD-RIGHT to return
;; the same result.
;; (= (fold-right #'* 1 (list 1 2 3))
;;    (fold-left  #'* 1 (list 1 2 3))) ==> T



;; Exercise 2.39

(defun fold-right-reverse (sequence)
  "Implementation of REVERSE using FOLD-RIGHT."
  (fold-right (lambda (x y)
		(append y (list x)))
	      nil
	      sequence))

(defun fold-left-reverse (sequence)
  "Implementation of REVERSE using FOLD-LEFT."
  (fold-left (lambda (x y)
	       (append (list y) x))
	     nil
	     sequence))




;;; Nested Mappings

(defun ordered-pairs (n)
  (accumulate #'append nil
	      (map 'list
		   (lambda (i)
		     (map 'list
			  (lambda (j) (list j i))
			  (enumerate-interval 1 (1- i))))
		   (enumerate-interval 1 n))))

;; above we combined the mapping and accumulating with APPEND. This is a common pattern
;; that we will isolate it as a separate procedure:

(defun flatmap (fn list)
  "The function given must map its input to a list type element, then the result is
APPENDed."
  (accumulate #'append nil
	      (map 'list fn list)))


;; using FLAT-MAP:
(defun 1-ordered-pairs (n)
  (flatmap (lambda (i)
	     (map 'list
		  (lambda (j) (list j i))
		  (enumerate-interval 1 (1- i))))
	   (enumerate-interval 1 n)))

;; filter for primes

(defun prime-sum? (pair)
  (primep (+ (first pair) (second pair))))

(defun make-pair-sum (pair)
  (let ((first (first pair))
	(second (second pair)))
    (list first second (+ first second))))

(defun prime-sum-pairs (n)
  ;; map
  (map 'list
       #'make-pair-sum
       ;; filter
       (filter #'prime-sum?
	       ;; enumerate
	       (1-ordered-pairs n))))

(defun %remove (item list)
  (filter (lambda (x) (not (= x item)))
	  list))

(defun permutations (list)
  (if (null list)
      (list nil) ;; if list is empty join the elements from the deferred chain with it
                 ;; using the CONS below. Ergo (cons <something> NIL) => (<something>)
      (flatmap (lambda (x) ;; runs 'list'-times, recursively on a smaller list then..
		 (map 'list
		      (lambda (p) (cons x p)) ;; the deferred chain stacks up (cons x ..)
		                              ;; the chain has always depth (length list)
		      (permutations (%remove x list))))
	       list)))


;; Exercise 2.40

(defun unique-pairs (n)
  ;; already implemented beforehand:
  (1-ordered-pairs n))

;; using UNIQUE-PAIRS in the implementation
(defun 1-prime-sum-pairs (n)
       (map 'list
	    #'make-pair-sum
	    (filter #'prime-sum?
		    (unique-pairs n))))


;; Exercise 2.41

(defun ordered-triples (n)
  "Returns triple i, j, k of ordered distinct integers smaller or equal to n."
  ;; triples require a third nested mapping
  (flatmap
   (lambda (i)
     (flatmap
      ;; now we map lists to a list and need to append the result twice - hence
      ;; using FLATMAP twice.
      (lambda (j)
	(map 'list
	     (lambda (k) (list k j i))
	     (enumerate-interval 1 (1- j))))
      (enumerate-interval 1 (1- i))))
   (enumerate-interval 1 n)))

(defun triple-sum-equal-s (n s)
  "Return triples of distinct ordered integers that are and smaller or equal to n
and whose sum is s."
  ;; enumerate our triples
  (filter (lambda (triple)
	    ;; the triple list form lends itself to be used as argument to APPLY
	    (= (apply #'+ triple) s))
	  (ordered-triples n)))


;; Exercise 2.42

;; The board will be a list, where (length list) = board-size
;; and each element will be a number whose position in the list
;; is the column and the magnitude the row.
;; example:
;; (list 1 1 2) represents the board
;; Q _ _         (list 1
;; Q _ _   ergo:       1
;; _ Q _               2)
(defparameter *empty-board* '())

(defun adjoin-position (new-row k rest-of-queens)
  "Add a new-row to the current n-board."
  (append rest-of-queens (list new-row)))


(defun board-diagonal-test (board-positions)
  "Tests if the last - (first (last board-positions)) - queen can attack any previously
placed queen diagonally. True if it can't attack."
  (let ((new-queen (first (last board-positions))))
    (loop for i in (reverse (butlast board-positions))
       ;; diagonal-delta is just the difference in row distance for each row we
       ;; move from the newly placed queen. Like a diagonal it increases by 1 per row.
       for diagonal-delta from 1
       :never
	 (or
	  ;; test "left" diagonal
	  (= (+ (- diagonal-delta) new-queen) i)
	  ;; test "right" diagonal
	  (= (+ diagonal-delta new-queen) i)))))

(defun save? (k positions)
  "True if the last queen in positions can't attack any other queen."
  (declare (ignore k)) ;; k isn't needed due to our board representation.
  (let  ((new-queen (first (last positions))))
    (if (and (loop for queen in (butlast positions)
		:never
		  (= queen new-queen))
	     (board-diagonal-test positions))
	t
	nil)))


;; A "position" is a set of rows of size k. k positions represent the full board.
(defun queen (board-size)
  "Solves the N Queen problem."
  (labels ((queen-cols (k)
	     (if (= k 0)
		 (list *empty-board*)
		 (filter
		  ;; returns all the positions that pass the test
		  ;; it is important that we filter here each of the
		  ;; board-sets with the newly added _row_ that branches
		  ;; to k-boards with a queen in the k-th position.
		  ;; The crucial part to understand is:
		  ;; this filter doesn't terminate the QUEEN-COLS procedure, but
		  ;; recursively adds new ROWS until there is (- k 1) no more at each step
		  ;; creating a k x [1-k] board that passes the SAVE?  test
		  (lambda (positions) (save? k positions))
		  (flatmap
		   (lambda (rest-of-queens)
		     (map 'list
			  ;; this mapping iterates through the column recursively
			  ;; creating k-boards with each column having a queen
			  ;; at the k-th position
			  (lambda (new-row)
			    (adjoin-position new-row k rest-of-queens))
			  (enumerate-interval 1 board-size)))
		   (queen-cols (- k 1)))))))
    (queen-cols board-size)))



