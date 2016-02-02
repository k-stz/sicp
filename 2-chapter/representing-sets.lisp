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
