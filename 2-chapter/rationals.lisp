(in-package :sicp)

(defun negativep (number)
  (< number 0))

;; constructor
;; exercise 2.1, better constructor
(defun make-rat (numerator denominator)
  (let ((g (gcd-m numerator denominator)))
    (if (negativep denominator)
	(cons (- (/ numerator g))
	      (- (/ denominator g)))
	(cons (/ numerator g)
	      (/ denominator g)))))

;; selectors
(defun numer (rational)
  (car rational))

(defun denom (rational)
  (cdr rational))


(defun print-rational (rational)
  (format t "~&~a/~a~%"
	  (numer rational)
	  (denom rational)))

;; arithmetic operations
(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))


(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; misc
(defvar *one-half* (make-rat 1 2))
(defvar *one-third* (make-rat 1 3))
