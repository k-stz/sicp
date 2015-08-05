(defpackage :newton-sicp (:use :cl))
(in-package :newton-sicp)


(defun avg (x y)
  "Return the average of x and y."
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


;; Some fixed points can be found by simply applying the function repeatedly on its own
;; output. And since we operate on a function we are introduced to higher-order use as
;; means of abstraction:

(defun close-enough? (x y)
  (< (abs (- x y)) 0.001))

(defun find-fixpoint (fn &optional (input 1.0))
  (let ((f-of-x (funcall fn input)))
    (if (close-enough? f-of-x input)
	f-of-x
	(find-fixpoint fn f-of-x))))

;; then we learn that we can reformulate the square root search for a fixpoint search:
;; x² = y  ; /y
;; x = y/x ;
;; f(x) = y/x  <- to remove the second variable, we can construct the function

(defun oscillating-fixpoint-sqrt (number)
  (let ((fn (lambda (x)
	      ;; we capture the number from input, effectively building a function
	      ;; suitable for a fixpoint-search
	      (/ number x))))
    (find-fixpoint fn)))

;; But the above function is an instance where the primitive fixpoint search doesn't work
;; by simple repeated application. The inputs and outputs will will oscillate over the
;; sqrt due to the nature of the function:
;; f (x) = y/x
;; repeated application
;; f (f (x)) ==> y/y/x  ==> y*x/y ==> x
;; adding another call just resets it:
;; f (*) ==> y/x
;; even deeper nesting:
;; y/y/y/y/x => y/y/y*x/y ==> y/y/x ==> y*x/y ==> x

;; We can solve this problem by a neat math trick called average-damping. Average damping
;; averages the input with the result (just like the first newton sqrt procedure did to
;; yield a improve-guess!). We can build an average damp version of any function:

(defun average-damp-function (fn)
  "Returns an average-damped version of the input function. Expects a single input."
  (lambda (x) (avg x (funcall fn x))))

;; now we can try it again:

(defun fixpoint-sqrt (number)
  (let ((fn (lambda (x)
	      ;; we capture the number from input, effectively building a function
	      ;; suitable for a fixpoint-search
	      (/ number x))))
    (find-fixpoint (average-damp-function fn))))
