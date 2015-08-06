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

(defun fixpoint (fn &optional (input 1.0))
  (let ((f-of-x (funcall fn input)))
    (if (close-enough? f-of-x input)
	f-of-x
	(fixpoint fn f-of-x))))

;; then we learn that we can reformulate the square root search for a fixpoint search:
;; x² = y  ; /y
;; x = y/x ;
;; f(x) = y/x  <- to remove the second variable, we can construct the function

(defun oscillating-fixpoint-sqrt (number)
  (let ((fn (lambda (x)
	      ;; we capture the number from input, effectively building a function
	      ;; suitable for a fixpoint-search
	      (/ number x))))
    (fixpoint fn)))

;; But the above function is an instance where the primitive fixpoint search doesn't work
;; by simple repeated application. The inputs and outputs will will oscillate over the
;; sqrt due to the nature of the function (we say: the function does not _converge_):
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

(defun average-damp (fn)
  "Returns an average-damped version of the input function. Expects a single input."
  (lambda (x) (avg x (funcall fn x))))

;; now we can try it again:

(defun fixpoint-sqrt (number)
  (let ((fn (lambda (x)
	      ;; we capture the number from input, effectively building a function
	      ;; suitable for a fixpoint-search (meaning: it will _converge_!)
	      (/ number x))))
    (fixpoint (average-damp fn))))


;; quick excursion on the golden ratio: Φ²= 1 + Φ or by dividing by Φ we get:  Φ = 1/Φ + 1
;; which is suitable for a fixpoint serach that yields the golden ratio:

(defun fixpoint-golden-ratio ()
  "Returns the golden ratio Φ."
  (fixpoint (lambda (x) (+ (/ 1 x) 1))))


;; back to average damping: This is a higher-order function that even returns an operator.
;; Due to cl being a lisp-2, we can't use the return value in function position:
;; Scheme ((average-damp) 10) ==> 55.0
;; CL (funcall (average-damp) 10) ==> 55.0

;; At this point we're supposed to reflect on the first newton method exposed to us: a
;; special case of the newton method (the general case can find roots for any
;; equations). We can immediately see a striking difference: before we averaged y with x/y
;; successively (1) which is just average-damping and the repeated application is (2) just
;; the way the fixpoint seach works!
;; In other words we abstracted a general procedure of averaging input and output values
;; of function (average-damp) and a general procedure for repeated application, with
;; the specialized goal of converging to a fixpoint (fixpoint-search)
;; (fixpoint (average-damp (lambda (x) (/ 2.0 x)))) ==> 1.4142135

;; finally abstracting these procedures out and exposing them as seperate entities allows
;; for simple reuse: we now can easily compute cube-roots:

(defun cube-root  (number)
  "Fixpoint search x = y / x²"
  (fixpoint (average-damp
	     (lambda (x) (/ number (expt x 2))))))


