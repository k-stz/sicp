(defpackage :sicp
  (:use :cl))

(in-package :sicp)

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
	 (end-segment (end-segment segment))
	 ))))
