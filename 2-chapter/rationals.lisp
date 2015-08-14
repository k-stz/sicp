(defpackage :sicp
  (:use :cl))

(in-package :sicp)

(defun make-rat (numerator denominator)
  (cons numerator denominator))

(defun numer (rational)
  (car rational))

(defun denom (rational)
  (cdr rational))

(defun print-rational (rational)
  (format t "~&~a/~a~%"
	  (numer rational)
	  (denom rational)))

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
