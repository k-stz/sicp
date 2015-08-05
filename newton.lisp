(defpackage :newton-sicp (:use :cl))
(in-package :newton-sicp)


(defun avg (x y)
  (/ (+ x y) 2.0))

(defun square (x) (* x x))

(defun newton-sqrt (radicand)
  (let ((initial-guess 1.0))
    (flet ((good-enough? (guess)
	     (< (abs (- (square guess)
			radicand))
		0.001))
	   (improve-guess (guess)
		 (avg guess (/ radicand guess))))
      (labels ((rec (guess)
		 (if (good-enough? guess)
		     guess
		     (rec (improve-guess guess)))))
	(rec initial-guess)))))
