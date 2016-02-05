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
				    (rest set))))))