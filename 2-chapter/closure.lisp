(defpackage :sicp (:use :cl)) (in-package :sicp)

;; Closure,
;; in abstract algebra refers to an operation that given a specific set of elements
;; as input returns elements of the set. We say "the elements are closed under an operation
;; if applying the operation on the elements produces elements of the same set".
;; Which is the case for the CONS operation, which always returns an object that
;; can be fed back into CONS.
;; Important distinction:
;; The lisp community, and by extension other programming languages by now, refer
;; to procedure with free variables as a closure:

;; (let ((x 2) (y 3))  ;; free variables
;;   (lambda (z)
;;     (+ z (incf x) (decf y)))) ;; returns a closure

;; SICP will not refer to the word "closure" in this sense.

;; Furthermore on the topic of "means of combination" one of the three key properties
;; of any programming languge is always expected to satisfy the closure property.
;; Some langauges do not provide this property, such as Basic (some version at least)
;; forces one to assemble elements into arrays, but the array is not allowed to contain
;; arrays themselves.


(defun list-ref (n list)
  "CL:NTH"
  (if (= n 0)
      (car list)
      (list-ref (1- n) (cdr list))))


;; Exercise 2.17

(defun last-pair (list)
  "CL:LAST"
  (if (null (cdr list))
      list
      (last-pair (cdr list))))

;; Exercise 2.18

(defun m-reverse (list)
  "CL:REVERSE"
  (labels ((rec (reverse-list list)
	     (if (null list)
		 reverse-list
		 (rec (cons (car list) reverse-list)
		      (cdr list)))))
    (rec () list)))


;; Exercise 2.19 - cc with list input

(defparameter *us-coins* (list 50 25 10 5 1))
(defparameter *uk-coins* (list 100 50 20 10 5 2 1 0.5))
(defparameter *eu-coins* (list 200 100 50 20 10 5 2 1))

(defun first-denomination (coins)
  (car coins))

(defun except-first-denomination (coins)
  (cdr coins))

(defun cc (amount coin-list)
  "Counting unique changes of <amount> with coins in <coin-list>."
  (labels ((rec (n coin-list)
	     (cond ((null coin-list) 0)
		   ((< n 0) 0)
		   ((= n 0) 1)
		   (t ;; else:
		    (+
		     ;; Try all coins in order given.
		     ;; preconditions eliminate this branch with (< n 0) and (= n 0)
		     (rec (- n (first-denomination coin-list)) coin-list)
		     ;; try all but the first kind of coin.
		     ;; the preconditions eliminate this branch with (null coin-list)
		       (rec n (except-first-denomination coin-list)))))))
    (if (<= amount 0)
	0
	(rec amount coin-list))))

;; The order of the coins doesn't affect the result. Our setup creates every combination
;; possible because we either branch of applying a denominiation or we branch of exempting
;; a denomination. Further branches then either operate on but one less denimination AND
;; on all denomination available. Or: the order in which we substract the coins doesn't matter
;; as long as we try all the coins. Example: Given coins 10 and 1 for amount 11 we either try
;; 10 then exempt 10 and then 1 yielding combiniation (10 1) for 11, or we first try 1 then
;; exempt 1 and then 10 also yielding combiniation (1 10) for 11. The order doesn't matter.


;; Exercise 2.20

;; Scheme: (define (foo x . lst) ...)
;; CL    : (defun  foo (x &rest lst) ...)
;;
;; Same is true for lambda function object definition
;; Scheme: ((lambda lst lst) 1 2 3) ==> (1 2 3)
;; CL    : ((lambda (&rest lst) lst) 1 2 3) ==> (1 2 3)
;;
;; Scheme: ((lambda (x . lst) (list 77 3 2 1))) ==> (77 (3 2 1))
;; CL    : ((lambda (x &rest lst) (list x lst)) 77 1 2 3) ==> (77 (1 2 3))

;; "parity" property of intergers to be either even or odd, "the parity of 2 is even" and
;; "n+1 has always the opposite parity of n if n is an integer"

(defun same-parity (&rest list)
  "Return a list of numbers that have the same parity as the first argument"
  (flet ((collect-odds ()
	   (loop for i in list
	      :when (oddp i)
	      :collect i))
	 (collect-evens ()
	   (loop for i in list
	      :when (evenp i)
	      :collect i)))
    (if (oddp (first list))
	(collect-odds)
	(collect-evens))))


;; no need to build the list from cdr to car and then m-reverse..
(defun 1-scale-list (list factor)
  "(cl:mapcar (lambda (x) (* x factor)) list)"
  (labels ((rec (cdr-list output-list)
	      (if (null cdr-list)
		  (m-reverse output-list)
		  (rec
		   (cdr cdr-list)
		   (cons (* factor (car cdr-list))
			 output-list)))))
    (let ((init-list '()))
      (rec list init-list))))


;; we can build the list from car to cdr recursively!
(defun 2-scale-list (list factor)
  "(cl:mapcar (lambda (x) (* x factor)) list)"
  (if (null list)
      '()
      (cons (* (car list) factor)
	    (2-scale-list (cdr list) factor))))


(defun m-map (fn list)
  "(cl:mapcar #'fn list)"
  (if (null list)
      '()
      (cons (funcall fn (car list))
	    (m-map fn (cdr list)))))

;; (m-map #'abs (list -10 2.5 -11.6 17)) ==> (10 2.5 11.6 17)

;; using M-MAP for the implementation
(defun 3-scale-list (list factor)
  (m-map (lambda (x) (* x factor))
	 list))


;; Exercise 2.21

(defun square (x) (* x x))

(defun 1-square-list (list)
  (if (null list)
      nil
      (cons (square (car list))
	    (1-square-list (cdr list)))))

(defun 2-square-list (list)
  (m-map #'square
	 list))


;; Exercise 2.22

;; An expression like
;;  (cons (square (car things))
;;        answer)
;; adds a new head to the list like so
;; (cons 'first nil) => (cons '2nd (cons 'first nil)) => (2nd first)

;; Interchanging the arguments:
;; (cons 'answer
;;       (cons 'square-etc))

;; puts the accumulator <answer> in the head and "replaces" the tail with the current
;; square.  Thereby violating the list property of a nil-terminator

;; (defun bad-square-list (items)
;;   (labels ((iter (things answer)
;; 		  (if (null things)
;; 		      answer
;; 		      (iter (cdr things)
;; 			    (cons answer
;; 				  (square (car things)))))))
;;     (iter items nil)))

;; (bad-square-list (list 1 2 3 4)) ==> ((((NIL . 1) . 4) . 9) . 16)


;; Exercise 2.23 - FOR-EACH

(defun for-each (fn list)
  (if (null list)
      nil
      (progn (funcall fn (car list))
	     (for-each fn (cdr list)))))


;;------------------------------------------------------------------------------

;; Trees and tree-recursiveness

(defun m-length (list)
  "CL:LENGTH"
  (labels ((rec (length list)
	     (if (null list)
		 length
		 (rec (1+ length)
		      (cdr list)))))
    (rec 0 list)))


(defun count-leaves (list)
  "Returns number of leaves in input list (tree"
  (cond ((null list)
	 0)
	((atom list) 1) ;; ATOM is equivalent to (not (consp x))
	(t ;; else
	 (+ (count-leaves (car list))
	    (count-leaves (cdr list))))))


;; Exercise 2.24

;; (list 1 (list 2 (list 3 4)))  ==> (1 (2 (3 4)))

;; is a list of two elements who's CDR is another list of two elements whose CDR
;; in turn is yet another list of two elements

                             ;; minor deviation for brevity's sake: a number in a box
                             ;; indicates a pointer to that number
;;         ^		     .---.---.    .---.---.
;;	  / \		     | 1 | ●-|--->| ● |NIL|
;;	 /   \		      --- ---      -|- ---
;;	/     \				    |
;;     1      /\			    ↓
;;	     /	\			  .---.---.    .---.---.
;;	    /	 \			  | 2 |	●-|--->| ● |NIL|
;;	   2	 /\			   --- ---      -|- ---
;;		/  \					 ↓
;;	       /    \				       .---.---.    .---.---.
;;	      3	     4				       | 3 | ●-|--->| 4 |NIL|
;;							--- ---      --- ---

;; we can verify the box-and-pointer notation by directly translating it to
;; nested CONS:
;; (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil)) ==> (1 (2 (3 4)))


;; Exercise 2.25

;; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9)))))) ==> 7
;; (car (car '((7)))) ==> 7
;; (car (cdr
;;       (car (cdr
;; 	    (car (cdr
;; 		  (car (cdr
;; 			(car (cdr
;; 			      (car (cdr ;; because ==> ((2 (3 (4 (5 (6 7))))))
;; 				    '(1 (2 (3 (4 (5 (6 7)))))))))))))))))) ==> 7


;; Exercise 2.26

;; (defparameter *x* (list 1 2 3))
;; (defparameter *y* (list 4 5 6))

;; (append *x* *y*) ==> (1 2 3 4 5 6)

;; not  ((1 2 3) . (4 5 6)) because then cdr would be ((4 5 6)) but it has to be
;; (4 5 6). The solution therefore is:
;; (cons *x* *y*) ==> ((1 2 3) 4 5 6)
;; It might be more helpful to think of cdr as "rest" part or "tail" part, and the
;; tail is usually bigger, more numerous in components, then the head.

;; (list *x* *y*) ==> ((1 2 3) (4 5 6))


;; Exercise 2.27

(defun deep-reverse (list)
  "cl:reverse on list and all its sublists"
  (labels ((rec (reverse-list list)
	     (cond ((null list) reverse-list)
		   ;; now we simply just have to test for non-cons occurance (ATOM list)
		   ;; because we opted for DEEP-REVERSE recursively
		   ((atom list) list)
		   (t ;; else
		    ;; simply applying DEEP-REVERSE on all CARs
		    (rec (cons (deep-reverse (car list)) reverse-list)
			 (cdr list))))))
    (rec () list)))


;; (deep-reverse '(9 (8 (7 6)) (5 (4 3 ((2))) 1) 0)) ==> (0 (1 (((2)) 3 4) 5) ((6 7) 8) 9)


;; Exercise 2.28

;; TODO: test if really _left to right order_ holds
(defun fringe (list)
  "Takes a tree represented by a list as input and returns a list of its leaves in
left to right order."
  (labels ((rec (leaves-list list)
	     (cond ((null list) leaves-list)
		   ;; ATOM is also true for the terminator NIL, but NULL already
		   ;; catches that case
		   ((atom list) 
		    (cons list leaves-list))
		   ((listp list)
		    (rec (append (fringe (car list)) leaves-list)
			 (cdr list))))))
    (rec () list)))


;; Exercise 2.29

;;         mobile
;;     branch  branch
;; length

(defun make-mobile (left right)
  (list left right))


(defun make-branch (length structure)
  (list length structure))


;; total-weight: 5
;; total-length: 7.5
(defparameter *test-mobile*
  (let* ((1x (make-branch 1 1))
	 (2y (make-branch 2 2))
	 (1x2ym (make-mobile 1x 2y))
	 (z (make-branch 2.5 1x2ym)))
    (make-mobile 2y z))
  "A bit mobile for tests")

;; a. selectors

(defun left-branch (mobile)
  (first mobile))

(defun right-branch (mobile)
  (second mobile))

(defun branch-length (branch)
  (first branch))

(defun branch-structure (branch)
  (second branch))

;; utils
(defun weight? (structure)
  (atom structure))

(defmacro with-branches (mobile (left-branch right-branch) &body body)
  `(let* ((mobile ,mobile) ; only once evaluated
	  (,left-branch (left-branch mobile))
	  (,right-branch (right-branch mobile)))
     ,@body))

;; b. calculate the total weight

(defun total-weight (mobile)
  "Return the total weight of the mobile's rods"
  (with-branches mobile (left-branch right-branch)
    (let ((left-structure (branch-structure left-branch))
	  (right-structure (branch-structure right-branch)))
      (+
       (if (weight? left-structure)
	   left-structure
	   ;; else it is a mobile
	   (total-weight left-structure))
       (if (weight? right-structure)
	   right-structure
	   (total-weight right-structure))))))

;; c.

(defun total-length (mobile)
  "Return the total length of the mobile's rods"
  (with-branches mobile (left right)
    (let ((left-length (branch-length left))
	  (right-length (branch-length right))
	  (left-structure (branch-structure left))
	  (right-structure (branch-structure right)))
      (+ left-length right-length
	 (if (weight? left-structure)
	     0
	     ;; else it is a mobile
	     (total-length left-structure))
	 (if (weight? right-structure)
	     0
	     (total-length right-structure))))))

(defparameter *balanced-mobile* '((2 3) (1 ((1 1.5) (1 1.5)))))

(defun mobile-balanced? (mobile)
  "A mobile is balanced if the product of each branch: weight x length, are equal. This
has to be true for any every submobile that a branch might contain at its structure part."
  (with-branches mobile (left right)
    (let ((left-structure (branch-structure left))
	  (right-structure (branch-structure right))
	  (left-length (branch-length left))
	  (right-length (branch-length right)))
      (and (= (* left-length
		 (if (weight? left-structure)
		     left-structure
		     (* (total-weight left-structure)
			(total-length left-structure))))
	      (* right-length
		 (if (weight? right-structure)
		     right-structure
		     (* (total-weight right-structure)
			(total-length right-structure)))))
	   ;; test the submobiles, if no submobiles --just weights--
	   ;; return T for the AND conditional
	   (if (weight? left-structure)
	       t
	       (mobile-balanced? left-structure))
	   (if (weight? right-structure)
	       t
	       (mobile-balanced? right-structure))))))

;; d. changing the mobile representation

(defun cons-make-mobile (left right)
  (cons left right))

(defun cons-make-branch (length structure)
  (cons length structure))

(defparameter *cons-test-mobile*
  (let* ((1x (cons-make-branch 1 1))
	 (2y (cons-make-branch 2 2))
	 (1x2ym (cons-make-mobile 1x 2y))
	 (z (cons-make-branch 2.5 1x2ym)))
    (cons-make-mobile 2y z)))

;; new selectors

(defun cons-left-branch (mobile)
  (car mobile))

(defun cons-right-branch (mobile)
  (cdr mobile))

(defun cons-branch-length (branch)
  (car branch))

(defun cons-branch-structure (branch)
  (cdr branch))

;; utils

(defmacro with-cons-branches (mobile (left-branch right-branch) &body body)
  `(let* ((mobile ,mobile) ; only once evaluated
	  (,left-branch (cons-left-branch mobile))
	  (,right-branch (cons-right-branch mobile)))
     ,@body))

;; d.b. calculate the total weight with CONS-representation mobile

(defun cons-total-weight (mobile)
  "Return the total weight of the mobile's rods"
  (with-cons-branches mobile (left-branch right-branch)
    (let ((left-structure (cons-branch-structure left-branch))
	  (right-structure (cons-branch-structure right-branch)))
      (+
       (if (weight? left-structure)
	   left-structure
	   ;; else it is a mobile
	   (cons-total-weight left-structure))
       (if (weight? right-structure)
	   right-structure
	   (cons-total-weight right-structure))))))

;; d.c.

(defun cons-total-length (mobile)
  "Return the total length of the mobile's rods"
  (with-cons-branches mobile (left right)
    (let ((left-length (cons-branch-length left))
	  (right-length (cons-branch-length right))
	  (left-structure (cons-branch-structure left))
	  (right-structure (cons-branch-structure right)))
      (+ left-length right-length
	 (if (weight? left-structure)
	     0
	     ;; else it is a mobile
	     (cons-total-length left-structure))
	 (if (weight? right-structure)
	     0
	     (cons-total-length right-structure))))))

(defparameter *cons-balanced-mobile* '((2 . 3) 1 (1 . 1.5) 1 . 1.5))

(defun cons-mobile-balanced? (mobile)
  "A mobile is balanced if the product of each branch: weight x length, are equal. This
has to be true for any every submobile that a branch might contain at its structure part."
  (with-cons-branches mobile (left right)
    (let ((left-structure (cons-branch-structure left))
	  (right-structure (cons-branch-structure right))
	  (left-length (cons-branch-length left))
	  (right-length (cons-branch-length right)))
      (and (= (* left-length
		 (if (weight? left-structure)
		     left-structure
		     (* (cons-total-weight left-structure)
			(cons-total-length left-structure))))
	      (* right-length
		 (if (weight? right-structure)
		     right-structure
		     (* (cons-total-weight right-structure)
			(cons-total-length right-structure)))))
	   ;; test the submobiles, if no submobiles --just weights--
	   ;; return T for the AND conditional
	   (if (weight? left-structure)
	       t
	       (cons-mobile-balanced? left-structure))
	   (if (weight? right-structure)
	       t
	       (cons-mobile-balanced? right-structure))))))


;;------------------------------------------------------------------------------

(defun scale-tree (tree factor)
  (cond ((null tree) nil)
	((atom tree) (* tree factor))
	(t ;; else
	 (cons (scale-tree (car tree) factor)
	       (scale-tree (cdr tree) factor)))))


(defun 1-scale-tree (tree factor)
  (m-map (lambda (x)
	   (if (atom x)
	       (* x factor)
	       (2-scale-tree x factor)))
	 tree))


;; Exercise 2.30

(defun square-tree-directly (tree)
  (cond ((null tree) nil)
	((atom tree) (square tree))
	(t ;; else
	 (cons (square-tree-directly (car tree))
	       (square-tree-directly (cdr tree))))))

;; using higher-order functions
(defun square-tree (tree)
  "Square the leaves of the input tree."
  (m-map (lambda (x)
	   (if (atom x)
	       (square x)
	       (square-tree x)))
	 tree))


;; Exercise 2.31

(defun tree-map (fn tree)
  "Maps function to tree."
  (m-map (lambda (x)
	   (if (atom x)
	       (funcall fn x)
	       (tree-map fn x)))
	 tree))


(defun 1-square-tree (tree)
  (tree-map #'square tree))


;; Exercise 2.32

(defun subsets (set)
  "Return all subsets of given set in a list."
  (if (null set)
      (list nil)
      (let ((rest (subsets (cdr set))))
	(append rest (map 'list
			  #'(lambda (x)
			      (cons (car set) x))
			  rest)))))


