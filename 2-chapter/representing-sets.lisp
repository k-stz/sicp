(in-package :sicp)


(defun element-of-set? (x set)
  (cond (;; nothing can be part of an empty set (element-of-set? NIL '()) should be false!
	 (null set) nil) 
	((equal? x (car set)) t)
	(t (element-of-set? x (cdr set)))))
