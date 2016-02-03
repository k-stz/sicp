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

(defun duplicate-element-of-set? (x set)
  (element-of-set? x set))

(defun duplicate-adjoin-set (x set)
  (adjoin-set x set))

;; here we deviate, we filter out multple elements from the
;; set-1 so we don't have to check an element then already passed or failed
;; the COND tests
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
