(defpackage :sicp
  (:use :cl))

(in-package :sicp)

;;; exercise 2.2

(defun make-point (x y)
  (cons x y))

(defun x-point (point)
  (car point))

(defun y-point (point)
  (cdr point))

(defun make-segment (start-point end-point)
  (cons start-point end-point))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cdr segment))

(defun print-point (point)
  (format t "~&(~a,~a)~%"
	  (x-point point)
	  (y-point point )))


(defun mid-point (segment)
  (let* ((start-segment (start-segment segment))
	 (end-segment (end-segment segment)))
    (make-point
     (newton-sicp:avg (x-point start-segment)
		      (x-point end-segment))
     (newton-sicp:avg (y-point start-segment)
		      (y-point end-segment)))))
