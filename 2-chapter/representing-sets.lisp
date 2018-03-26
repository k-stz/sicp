(in-package :sicp)


(defun element-of-set? (x set)
  (cond (;; nothing can be part of an empty set (element-of-set? NIL '()) should be false!
	 (null set) nil) 
	((equal? x (car set)) t)
	(t (element-of-set? x (cdr set)))))


(defun adjoin-set (x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(defun intersection-set (set-1 set-2)
  (cond ((or (null set-1) (null set-2)) '())
	((element-of-set? (car set-1) set-2)
	 (cons (car set-1)
	       (intersection-set (rest set-1)
				 set-2)))
	(t
	 (intersection-set (rest set-1)
			   set-2))))


;; exercise 2.59

(defun union-set (set-1 set-2)
  (cond ((null set-1) set-2)
	((null set-2) set-1)
	((not (element-of-set? (car set-1) set-2))
	 (union-set (rest set-1) (cons (car set-1) set-2)))
	(t (union-set (rest set-1) set-2))))


;; exercise 2.60 - sets with duplicates

;; same efficiency
(defun duplicate-element-of-set? (x set)
  (element-of-set? x set))

;; efficiency: O(1)
(defun duplicate-adjoin-set (x set)
  (cons x set))

;; here we deviate, we filter out multiple elements from the
;; set-1 so we don't have to check an element which already passed or failed
;; the COND tests
;; efficiency: the filter adds n more predicate tests where n is the length of the set-1
;; while this has to be filtered anew on each recursion down the set-1 so we added n*n
;; time, ergo: O(n²)
(defun duplicate-intersection-set (set-1 set-2)
  (let ((filtered-set-1
	 (filter #'(lambda (x) (not (equal? x (car set-1))))
		 (rest set-1))))
    (cond ((or (null set-1) (null set-2)) '())
	  ((duplicate-element-of-set? (car set-1) set-2)
	   (cons (car set-1)
		 (duplicate-intersection-set
		  filtered-set-1
		  set-2)))
	  (t
	   (duplicate-intersection-set filtered-set-1
				       set-2)))))

;; efficiency: doesn't use any predicates and only conses two lists together
;;             which is negligible or at most O(n)
(defun duplicate-union-set (set-1 set-2)
  (append set-1 set-2))


;; sets as ordered lists?
;; by doing that we will gain efficiency on some operations

;; Careful, now only works on sets of numbers. We'd need to add a rule
;; of how to order any symbols (lexicographically or a unique representation
;; like the reference addresss).
(defun ordered-element-of-set? (x set)
  "Is x an element of the set? Operates on a set with an ordered representation."
  (cond ((null set) nil)
	((= x (car set)) t)
	;; we can stop the recursion knowing that subsequent
	;; elements will be all bigger than the one we're looking for
	((< x (car set)) nil)
	(t (ordered-element-of-set? x
				    (rest set)))))

;; though on average we save n/2 steps, that is on best case the first item is
;; the one we're looking for, in worst case the last. In landau-notation, order
;; of growth, that is still O(n), but on average it is better than the unordered
;; representation

(defun ordered-intersection-set (set-1 set-2)
  (if (or (null set-1) (null set-2))
      '()
    (let ((x1 (first set-1)) (x2 (first set-1)))
      (cond ((= x1 x2)
	     (cons x1
		   (intersection-set (rest set-1)
				     (rest set-2))))
	    ((< x1 x2)
	     ;; x1 can't be a member of set-2 cause x2 is the
	     ;; smallest element in set-2, that's why we take the next
	     ;; element in set-1:
	     (intersection-set (cdr set-1) set-2))
	    ((< x2 x1)
	     ;; analog for x2 in set-2
	     (intersection set-1 (cdr set-2)))))))

;; efficiency of ORDERED-INTERSECTION-SET:
;; we recurse down the two sets, and each of the brances either recurses down both or one
;; of the lists. That means that the time is at worst the sum of the (+ (length set-1)
;; (length set-2)) and at best, recursing down both, the half of it - a linear complexity:
;; O(n)
;; A tremendous improvement over the unordered operatation with O(n²) which scanned one set
;; with each element of the other set, that is n*n-times

;; exercise 2.61 - implementing ADJOIN-SET on ordered sets, which requires on average
;;                 half the steps as opposed to the unordered set operation

(defun ordered-adjoin-set (x set)
  "ADJOIN-SET, expects an ordered set as input, and only numerical elements."
  (cond ((null set) nil)
	((= x (car set)) nil)
	((> x (car set)) (cons (car set) (ordered-adjoin-set x (rest set))))
	((< x (car set)) (cons x set))))



;; exercise 2.62
;; two parallel finite lines represent the two sets you run across one and jump over to
;; the other when it contains smaller elements - linear growth
(defun ordered-union-set (set-1 set-2)
  (let ((s1 (car set-1))
	(s2 (car set-2)))
    (cond ((null set-1) set-2)
	  ((null set-2) set-1)
	  ((= s1 s2)
	   (cons s1
		 (ordered-union-set (rest set-1)
				    (rest set-2))))
	  ((< s1 s2)
	   (cons s1 (ordered-union-set (rest set-1)
				       set-2)))
	  ((> s1 s2)
	   (cons s2 (ordered-union-set set-1
				       (rest set-2)))))))


;;; Sets as binary trees
;; we can get even more efficient by representing the set with a binary
;; tree, where the left subtree only contains elements smaller than its
;; node and the right subtree larger once.
;; As we move down the tree, in search, we half the problem size (with
;; balanced tree) - a distinguishing characteristic of logarithmic growth
;; Θ(log n)

;; implementation of binary trees:
;; a tree shall be represented by a three element list, the first element
;; is the node the 2nd the left subtree the 3rd the right one.
;; a nil in any subtree field represents that there is none connected there.

(defun make-tree (entry left right)
  (list entry left right))

(defun entry (tree)
  (first tree))

(defun left-branch (tree)
  (second tree))

(defun right-branch (tree)
  (third tree))


(defun tree-element-of-set? (x set)
  (cond ((null set) nil)
	((= x (entry set)) t)
	((< x (entry set))
	 (tree-element-of-set? x (left-branch set)))
	((> x (entry set))
	 (tree-element-of-set? x (right-branch set)))))

;; again a Θ(log n) operation
(defun tree-adjoin-set (x set)
  (cond ((null set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 ;; wow that's new, we recurse down the tree - cutting it down -
	 ;; and growing it anew behind us!
	 (make-tree (entry set)
		    (tree-adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (tree-adjoin-set x (right-branch set))))))


;; the advantage of the binary tree rest on the property of it staying
;; balanced. Imagine adding the elements 1 through 7 to an empty tree,
;; and you get a tree with no left branches, it would be a straigt
;; line of nodes visually. We can plainly see that we lost the Θ(log n)
;; in there.
;; We can solve this problem by divising an operationg that transforms
;; an unbalanced tree back into a balanced one, or we can use a
;; datastructure with the O(log n) property (for insertion and searching
;; elements) such as:
;; B-Trees and red-black trees.


;; exercise 2.63
;; The following two procedures translate a tree to a list, the question is
;; whether they both produce the same list every time.

(defvar *tree-2.16-a*
  (make-tree 7
	     (make-tree 3
			(make-tree 1 nil nil)
			(make-tree 5 nil nil))
	     (make-tree 9
			nil
			(make-tree 11 nil nil))))

(defvar *tree-2.16-b*
  (make-tree 3
	     (make-tree 1 nil nil)
	     (make-tree 7
			(make-tree 5 nil nil)
			(make-tree 9
				   nil
				   (make-tree 11 nil nil)))))

(defvar *tree-2.16-c*
  (make-tree 5
	     (make-tree 3
			(make-tree 1 nil nil)
			nil)
	     (make-tree 9
			(make-tree 7 nil nil)
			(make-tree 11 nil nil))))

(defun tree->list-1 (tree)
  "Take a tree and return a list of its entries (each node has an entry)."
  (if (null tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(defun tree->list-2 (tree)
  "Take a tree and return a list of its entries (each node has an entry)."
  (labels ((copy-to-list (tree result-list)
	     (print (entry tree))
	     (if (null tree)
		 result-list
		 (copy-to-list (left-branch tree)
			       (cons (entry tree)
				     (copy-to-list (right-branch tree)
						   result-list))))))
    (copy-to-list tree '())))

;; `tree->list-1' traverse the tree recursively, down the left-branch, until it hits a
;; dead end, only then it recurses down the deferred chain of right-branches. In effect
;; it either prepends (cons (entry tree)) of the left-branch entries in front of it,
;; or appends (cons (entry (left-branch tree))) in front of it.

;; `tree->list-2', on the other hand, recurses down the right-branch first. As the final
;; `copy-to-list' call is performed on the (right-branch tree). The recursion terminates
;; on first hitting a nil node in a right-branch. Then the `result-list' will be a chain
;; of the right-branch (entry-tree nodes). Prepended to this is finally a (right-branch
;; tree) recursion which looks like this (cons (entry (right-branch tree)) result-list).
;; Now we can see that this procedure works the other way around.

;; Complexity:

;; `tree->list-1' visits every node once calling APPEND on it
;;  given SICP's definition of APPEND it is a O(log n) operation
;;  therefore the total order of growth of this function is O(n log n)

;; `tree->list-2' visits every node once calling a CONS on it => O(n)

;; a) For the procedures tree->list-1 and tree->list-2 what is the result of when
;; applied on the trees from exercise 2.16, and if not how do they differ?

;; Answer: Both functions have the same output for all the tree: *tree-2.16-a*, *tree-2.16-b* and
;; *tree-2.16-c*

;; b) Do they have the same order of growth? If not which one grows more slowly?

;; Answer: tree->list-2 visits each node exactly once, recursively while calling CONS on it
;;         giving it the complexity O(n)
;;         tree->list-1 while returning from its recursion to any root node, always APPENDs the
;;         entries of the left-branch to it. APPEND will have to linearly traverse the sublist
;;         and each list depending on the depth in the tree will be a certain half size, making
;;         it a log n operation. Paired with the visiting of each node, we get O(n log n)

