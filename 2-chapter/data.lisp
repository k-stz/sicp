(defpackage :sicp (:use :cl))
(in-package :sicp)


;; Data is just some collection of constructors and selectors some conditions
;; they must fulfill to be a valid representation

;; a pair, here represented by a closure, a function closing over the input that
;; will depending on its input retrieve the closed over data. 

(defun m-cons (x y)
  (lambda (i)
    (cond ((= i 0) x)
	  ((= i 1) y)
	  (t (error "Argument not 0 or 1 -- CONS")))))

(defun m-car (m-cons)
  (funcall m-cons 0))

(defun m-cdr (m-cons)
  (funcall m-cons 1))
